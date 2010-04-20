package de.immaterialien.sturmonanny.fbdjhosting


import de.immaterialien.sturmonanny.util.Logging


class FbdjHost(val conf : de.immaterialien.sturmonanny.core.Configuration)  extends Logging {
		var outList : java.util.LinkedList[String] = null 
		var inList : java.util.LinkedList[String] = null
  
  //, conf.fbdj.overridesJar, conf.fbdj.fbdjConfiguration.apply
  
		private val jarFile = new java.io.File(conf.fbdj.installationPath+"/FBDj.jar")
  	private val jarUrl = jarFile.toURL
   
    
  	private val overrideFile = new java.io.File(conf.fbdj.overridesJar)
  	private val overrideUrl = overrideFile.toURL
    private val configurationPath:String = conf.fbdj.fbdjConfiguration

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
   
  	try{
  		val connClass = classLoader.loadClass("utility.SocketConnection")
  		val inQueueField = connClass.getField("inQueue")
  		val outQueueField = connClass.getField("outQueue")
  		
  		outList = inQueueField.get(null).asInstanceOf[java.util.LinkedList[String]]
  		inList = outQueueField.get(null).asInstanceOf[java.util.LinkedList[String]]
debug("FbdjHost: outList : "+System.identityHashCode(outList))    
debug("FbdjHost: inList  : "+System.identityHashCode(inList))    
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
				    arg("dcgScMissionTimeMinutes", ""+conf.fbdj.DCG.minutesPerMission.apply), 
				    arg("headless", ""+conf.fbdj.headless.apply), 
				    arg("stats", ""+conf.fbdj.stats.apply), 
				    arg("autoconnect", ""+conf.fbdj.autoconnect.apply), 
		  	    arg("dcgPath", ""+conf.fbdj.DCG.dcgPath.apply), 
		  
				    arg("config", configurationPath), 
				    arg("installationPath", conf.fbdj.installationPath) 
		))
  
		val threadName = "FBDj with "+parameters(0).mkString(" ")
  
		val tg = new ThreadGroup("group for "+threadName)
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
      if(thread!=null) thread interrupt
    }

	
}
