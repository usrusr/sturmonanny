package de.immaterialien.sturmonanny.fbdjhosting

import de.immaterialien.sturmonanny.core
import de.immaterialien.sturmonanny.util.Logging
import scala.collection.immutable
import java.net.URL
import net.liftweb.actor.LiftActor
import java.util.concurrent._
object ContainerPool {

  def returnToPool(what:FbdjContainer){
	  poolManager ! what
  }
  def getContainer(installationPath:String, overridesJar : String) : FbdjContainer = {
	  val ret = poolManager !? InstallationInfo(installationPath , overridesJar)
    ret.asInstanceOf[FbdjContainer]
  }
  private object poolManager extends LiftActor with Logging{
	  private var overridesToPool : Map[InstallationInfo, List[FbdjContainer]] = Map.empty
   
	  def messageHandler = {
	    case info : InstallationInfo => {
debug("getting from pool "+info)		      
	      val opt = overridesToPool.get(info)
        if(opt.isDefined){
      	  val list = opt.get
          if(list.isEmpty) {
debug("new container "+info + " when pool is empty: "+overridesToPool)		      
            
            reply(new FbdjContainer(info))
          } else {
            val ret = list.head
            overridesToPool += info -> list.tail
debug("existing container "+info + " then pool is: "+overridesToPool)		            
            reply(ret)
          }
        }else{
debug("new container "+info + " when pool is very empty: "+overridesToPool)		      
          reply(new FbdjContainer(info)) 
        }
	    }
	    case back : FbdjContainer => {
debug("container pool returning "+back.info)	      
	   	 	val opt = overridesToPool.get(back.info)
        if(opt.isDefined){
          overridesToPool += back.info -> (back :: opt.get)
        }else{
          overridesToPool += back.info -> (back :: Nil)
        }
	    } 
debug("container pool returning -> "+overridesToPool)	     
	  }
  }
  
	case class InstallationInfo(installationPath:String, overridesJar : String)
}

class FbdjContainer(val info:ContainerPool.InstallationInfo) extends Logging {
		val overrideUrl = new java.io.File(info.overridesJar).toURL
  
  override def toString = "FBDj container @ "+info.installationPath+" from "+info.overridesJar
  
		var outList : java.util.LinkedList[String] = null 
		var inList : java.util.LinkedList[String] = null
  
		private var conf : de.immaterialien.sturmonanny.core.Configuration = null
		private var name:String = "def"
  
		def changeConfiguration(nConf : core.Configuration, nName:String){
		  if(nConf ne conf){
		    conf = nConf
		  }
		  name=nName
		}

  debug("initializing fbdj container @ '"+info.installationPath +"' \n  "+FbdjHost.loaderstatus)
		private val jarFile = new java.io.File(info.installationPath+"/FBDj.jar")
  	private val jarUrl = jarFile.toURL
    
    
  	private val overrideFile = new java.io.File(info.overridesJar)


   	if( ! jarFile.canRead || ! overrideFile.canRead ){
   	  var list : List[String] = Nil
   	  if( ! jarFile.exists){
   	    list ::= "FBDj jar '"+jarFile.getAbsoluteFile+"' does not exist"
   	  }else if( ! jarFile.canRead){
   	    list ::= "FBDj jar '"+jarFile.getAbsoluteFile+"' cannot be read"
   	  } 
   	  if( ! overrideFile.exists){
   	    list ::= "FBDj overrides '"+overrideFile.getAbsoluteFile+"' do not exist"
   	  }else if( ! overrideFile.canRead){
   	    list ::= "FBDj overrides '"+overrideFile.getAbsoluteFile+"' cannot be read"
   	  } 
      
      if( ! list.isEmpty) throw new java.io.FileNotFoundException(list.mkString(" and ")+", please check your configuration") 
   	}
   

  	val parent = this.getClass.getClassLoader
   
  	try{
  		parent.loadClass("main.FBDj")
  		throw new IllegalStateException("Error starting FBDj container: FBDj.jar must not be on the classpath, it should only be referenced from the sturmonanny configuration!")
    }catch{
      case _:ClassNotFoundException => // check passed, no FBDj.jar on classpath
    }
    
  	val classLoader = new java.net.URLClassLoader(Array(overrideUrl, jarUrl), parent)
   FbdjHost.loaderstatus.registerLoader(classLoader)
  	try{
  		val connClass = classLoader.loadClass("utility.SocketConnection")
  		val inQueueField = connClass.getField("inQueue")
  		val outQueueField = connClass.getField("outQueue")
  		
  		outList = inQueueField.get(null).asInstanceOf[java.util.LinkedList[String]]
  		inList = outQueueField.get(null).asInstanceOf[java.util.LinkedList[String]]
trace("FbdjHost: outList : "+System.identityHashCode(outList))    
trace("FbdjHost: inList  : "+System.identityHashCode(inList))    
  		()
    }catch{
      case c:ClassNotFoundException => throw new ClassNotFoundException("Could not load socket connection override from "+jarUrl+" ")
    }

   	val threadName = "FBDj container"
    val tg = new ThreadGroup("group for "+threadName+" "+FbdjHost.count) {
		  override def uncaughtException(t:Thread , e:Throwable ) {
		    if(classOf[java.lang.ThreadDeath].isInstance(e)){
debug("thread death in "+t.getName)		      
		    }else{
		      super.uncaughtException(t, e)
		    }
		  }
		}
  	val mainClass = try{
  		classLoader.loadClass("de.immaterialien.sturmonanny.fbdjinterface.StartStopInterface")
    }catch{
      case c:ClassNotFoundException => throw new ClassNotFoundException("Could not load FBDj.jar from "+jarUrl+" ")
    }
		val internalInterface = mainClass.newInstance.asInstanceOf[javax.xml.ws.Provider[String]]
  
		val interface = new Thread(tg, threadName) with javax.xml.ws.Provider[String]{
		  val nIn = new Object
		  val nOut = new Object
		  val in = new SynchronousQueue[Option[String]]
		  val out = new SynchronousQueue[Option[String]]
		  override def run = {
		    try{
				  while(true) {
				    val arg = in.take
debug("post-threadwall : "+arg)
				    val ret = if(arg.isDefined) internalInterface.invoke(arg.get)
				    					else null
debug("out from behind threadwall return "+ret)
				    if(ret==null) out.put(None)
				    else out.put(Some(ret))
			    }
        }catch{
          case e=> {
            debug("FBDJ interface: "+e.getMessage,e)
            out.put(Some(e .getClass.getSimpleName+": "+ e.getMessage))
          }
        }
      }
		  override def invoke(arg:String) = {
				out.poll
				if(arg==null) null else {
debug("pre-threadwall : "+arg)
			    in.put(Some(arg))
			    //val ret = out.poll(10, TimeUnit.SECONDS)
	        val ret = out.take
debug("emerged from threadwall return "+ret.getOrElse(null))
		    	ret.getOrElse(null)
		    }
		  }
		}
		interface.setContextClassLoader(classLoader)
		interface.start
		object send {
		  def MISSION_CREATION_COMMAND_LINE(arg:String) = interface.invoke("MISSION_CREATION_COMMAND_LINE="+arg)
			def HEADLESS(arg:String) = interface.invoke("HEADLESS="+arg)
			def INSTALLATION_PATH(arg:String) = interface.invoke("INSTALLATION_PATH="+arg)
			def MIN_SORTIES_SMALLER(arg:String) = interface.invoke("MIN_SORTIES_SMALLER="+arg)
			def MIN_SORTIES_BIGGER(arg:String) = interface.invoke("MIN_SORTIES_BIGGER="+arg)
			def MIN_PILOTS_SMALLER(arg:String) = interface.invoke("MIN_PILOTS_SMALLER="+arg)
			def MIN_PILOTS_BIGGER(arg:String) = interface.invoke("MIN_PILOTS_BIGGER="+arg)
			def DCGPATH(arg:String) = interface.invoke("DCGPATH="+arg)
			def CONFIGURATION(arg:String) = interface.invoke("CONFIGURATION="+arg)
	    def NAME(arg:String) = interface.invoke("NAME="+arg)
     
			def START = interface.invoke("START=true")
			def STOP = interface.invoke("STOP=true")
			def DISCONNECT = interface.invoke("DISCONNECT=true")
			def CONNECT = interface.invoke("CONNECT=true")
			def RESET = interface.invoke("RESET=true")
		}                                                 
		                                                   
//  "CONNECT="+conf.fbdj) 

//		interface.invoke("LAUNCH="+conf.fbdj..apply)
//		interface.invoke("DISCONNECT="+conf.fbdj..apply)
//		interface.invoke("CONNECT="+conf.fbdj..apply)
//		interface.invoke("RESET="+conf.fbdj..apply)
                     
		def start = {
		  send RESET;
			send MISSION_CREATION_COMMAND_LINE ""+conf.fbdj.DCG.dcgCommand.apply
			send HEADLESS ""+conf.fbdj.headless.apply
			send INSTALLATION_PATH ""+conf.fbdj.installationPath.apply
			send MIN_SORTIES_SMALLER ""+conf.fbdj.DCG.campaignProgress.minSorties.smaller.apply
			send MIN_SORTIES_BIGGER ""+conf.fbdj.DCG.campaignProgress.minSorties.bigger.apply
			send MIN_PILOTS_SMALLER ""+conf.fbdj.DCG.campaignProgress.minPilots.smaller.apply
			send MIN_PILOTS_BIGGER ""+conf.fbdj.DCG.campaignProgress.minPilots.bigger.apply
			send DCGPATH ""+conf.fbdj.DCG.dcgPath.apply
			send CONFIGURATION ""+conf.fbdj.fbdjConfigurationDirectory.apply
			send NAME name
			
			send.START
    }
               
		def connect(onoff : Boolean){
		  if(onoff) send.CONNECT
		  else send.DISCONNECT
		}
  
    def stop = {
			send.STOP
      ContainerPool.returnToPool(this)
      /*
debug("shutting down FBDj!!! "+FbdjHost.loaderstatus)
tg.list
      val parameters :Array[Array[String]]= Array(Array(
        arg("disconnect", ""+true),
		    arg("config", configurationPath), 
		    arg("installationPath", conf.fbdj.installationPath)         
      ))
      try{
      	mainMethod.invoke(null, parameters:_*)
      }catch{
        case i:java.lang.reflect.InvocationTargetException if
          (i.getCause!=null 
           && i.getCause.getClass == classOf[java.util.concurrent.RejectedExecutionException] 
           && i.getCause.getMessage.contains("disconnect")) => ()
      }
//      if(tg!=null) {
//        
//        while(tg.activeCount>0){
//        tg.interrupt
//        try{Thread.sleep(1000)}catch{case _=>()}
//debug("active threads in group:  "+tg.activeCount)        
////      	  tg.stop
//        }
//      }
      FbdjHost.loaderstatus.unregisterLoader(classLoader)
debug("shutting down FBDj, classloader unregistered "+FbdjHost.loaderstatus)
      */
    }

}
