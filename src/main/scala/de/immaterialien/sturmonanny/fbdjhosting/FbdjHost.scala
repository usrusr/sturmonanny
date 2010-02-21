package de.immaterialien.sturmonanny.fbdjhosting


import de.immaterialien.sturmonanny.util.Logging


class FbdjHost(val fbdjInstallation : String, overridesPath : String, configurationPath:String)  extends Logging {
		var outList : java.util.LinkedList[String] = null
		var inList : java.util.LinkedList[String] = null
  
		private val jarFile = new java.io.File(fbdjInstallation+"/FBDj.jar")
  	private val jarUrl = jarFile.toURL
   
    
  	private val overrideFile = new java.io.File(overridesPath)
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
   
  	try{
  		val connClass = classLoader.loadClass("utility.SocketConnection")
  		val inQueueField = connClass.getField("inQueue")
  		val outQueueField = connClass.getField("outQueue")
  		
  		outList = inQueueField.get(null).asInstanceOf[java.util.LinkedList[String]]
  		inList = outQueueField.get(null).asInstanceOf[java.util.LinkedList[String]]
//debug("outList : "+outList)    
//debug("inList : "+inList)    
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
  
		val parameters :Array[Array[String]]= Array(Array(
				    "config="+configurationPath, 
				    "installationPath="+fbdjInstallation
		))
		val thread = new Thread("FBDj with "+parameters(0).mkString(" ")) {
		  override def run = {
debug("starting "+this.getName)
				  mainMethod.invoke(null, parameters:_*)
	//			  mainMethod.invoke(null, Array(args):_* )
      }
		}
		thread run
    
    def stop = {
      if(thread!=null) thread interrupt
    }

	
}
