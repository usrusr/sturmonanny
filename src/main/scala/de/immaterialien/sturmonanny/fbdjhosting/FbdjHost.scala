package de.immaterialien.sturmonanny.fbdjhosting


import de.immaterialien.sturmonanny.util.Logging

object FbdjHost {
  var count = 0
  object loaderstatus {
	  val loaders = new java.util.WeakHashMap[java.net.URLClassLoader, Boolean]
	  def registerLoader(loader:java.net.URLClassLoader) = loaders.put(loader, true)
	  def unregisterLoader(loader:java.net.URLClassLoader) = loaders.put(loader, false)
	  override def toString = { 
	    var actives = 0
	    var zombies = 0
	    val it = loaders.entrySet.iterator
      while(it.hasNext){
        val entry = it.next
        val ref = entry.getKey
        val active = entry.getValue
        if(active) actives=1+actives else zombies=1+zombies 
      }
      ("\n   (of "+(zombies+actives)+" created classloaders, "+zombies+" are zombies)")
	  }
  }
}
class FbdjHost(val conf : de.immaterialien.sturmonanny.core.Configuration)  extends Logging {
		var outList : java.util.LinkedList[String] = null 
		var inList : java.util.LinkedList[String] = null
  
  //, conf.fbdj.overridesJar, conf.fbdj.fbdjConfiguration.apply
  	val isntallationPath = conf.fbdj.installationPath.apply
    private val configurationPath:String = conf.fbdj.fbdjConfigurationDirectory.apply
  debug("initializing fbdj container @ '"+isntallationPath +"' configured at '"+configurationPath+"'\n  "+FbdjHost.loaderstatus)
		private val jarFile = new java.io.File(isntallationPath+"/FBDj.jar")
  	private val jarUrl = jarFile.toURL
    
    
  	private val overrideFile = new java.io.File(conf.fbdj.overridesJar)
  	private val overrideUrl = overrideFile.toURL

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
   
  	// load main class
  	val mainClass = try{
  		classLoader.loadClass("main.FBDj")
    }catch{
      case c:ClassNotFoundException => throw new ClassNotFoundException("Could not load FBDj.jar from "+jarUrl+" ")
    }
		val mainMethod = mainClass.getMethod("main", classOf[Array[String]])
  
		private def arg(name:String, value:String) = name+"=\""+value+"\"" 
  
		val parameters :Array[Array[String]]= Array(Array(
//	"missionCreationCommandLine";
//	"dcgScMissionTimeMinutes";
//	"headless";
//	"installationPath";
//	"stats";		  
				    arg("missionCreationCommandLine", conf.fbdj.DCG.dcgCommand), 
//				    arg("dcgScMissionTimeMinutes", ""+conf.fbdj.DCG.minutesPerMission.apply), 
				    arg("headless", ""+conf.fbdj.headless.apply), 
//				    arg("stats", ""+conf.fbdj.stats.apply), 
//				    arg("autoconnect", ""+conf.fbdj.autoconnect.apply), 
		  	    arg("dcgPath", ""+conf.fbdj.DCG.dcgPath.apply), 
 
            arg("minSortiesBigger", ""+conf.fbdj.DCG.campaignProgress.minSorties.bigger.apply),
            arg("minSortiesSmaller", ""+conf.fbdj.DCG.campaignProgress.minSorties.smaller.apply),
            arg("minPilotsBigger", ""+conf.fbdj.DCG.campaignProgress.minPilots.bigger.apply),
            arg("minPilotsSmaller", ""+conf.fbdj.DCG.campaignProgress.minPilots.smaller.apply),

				    arg("config", configurationPath), 
				    arg("installationPath", conf.fbdj.installationPath) 
		))
  
		FbdjHost.count+=1
		val threadName = "FBDj "+FbdjHost.count+" with "+parameters(0).mkString(" ")
  
		val tg = new ThreadGroup("group for "+threadName+" "+FbdjHost.count) {
		  override def uncaughtException(t:Thread , e:Throwable ) {
		    if(classOf[java.lang.ThreadDeath].isInstance(e)){
debug("thread death in "+t.getName)		      
		    }else{
		      super.uncaughtException(t, e)
		    }
		  }
		}
		val thread = new Thread(tg, threadName) {
		  override def run = {
debug("starting "+this.getName + " with\n "+parameters(0).mkString("\n "))
				  mainMethod.invoke(null, parameters:_*)
	//			  mainMethod.invoke(null, Array(args):_* )
      }
		}
  
  
		
		thread.setContextClassLoader(classLoader)
		thread.start
    
    def stop = {
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
    }

	
}
