package de.immaterialien.sturmonanny.global

import _root_.de.immaterialien.sturmonanny.util.Logging
import _root_.de.immaterialien.sturmonanny.util.configgy
import scala.collection.mutable
import scala.collection.immutable
import _root_.de.immaterialien.sturmonanny.core 

class Instances(val fname:String, val instancesMapToLoad : mutable.Map[String, core.Server]) extends configgy.ConfigurationSchema(fname) with configgy.LiftSupport {
  def this()=this("instances.conf", null)
  def this(fname:String)=this(fname, null) 
  def this(instancesMapToLoad : mutable.Map[String, core.Server])=this("instances.conf", instancesMapToLoad)
  doc = """list of the internal sturmonanny instances""" 
	object instances extends Table("default.conf") { 
	  doc = """list paths to configuration files defining the various sturmonanny instances running in this JVM
example: 
server1 = "server1.conf"
server2 = "server2.conf" 
""" 
	}
  override def readConfiggy(in:net.lag.configgy.Config)={
    super.readConfiggy(in)
println("instances read: "+this+"\n---from---"+in)
    if(instancesMapToLoad!=null) instancesMapToLoad.retain((k,v)=>{
println("found in map: "+k+" is "+v)
      if((instancesMapToLoad!=null) && (instancesMapToLoad contains k)){
println("keeping "+k)
        true
      }else{
println("dropping "+k)
        if(instancesMapToLoad!=null) instancesMapToLoad(k).shutdown        
        false
      }
    })
    
    initInstances()
  }
  private def newServer(name:String, conf:String)={
  		var server :  de.immaterialien.sturmonanny.core.Server= null 
      try{ 
println("go create... "+name+": "+conf+" in "+new java.io.File(conf).getAbsolutePath)        
        server = core.Server.withThreadGroup(conf)
println("created server for "+name+": "+server)        
        if(instancesMapToLoad!=null) instancesMapToLoad.put(name, server)
        
      }catch {
        case x : java.io.FileNotFoundException => status.error(name+": file "+conf+" was not found ("+new java.io.File(conf).getAbsolutePath+")")
        case x : java.io.IOException => status.error(name+": could not read "+conf)
        case pe : net.lag.configgy.ParseException => pe.getCause match {
	        case x : java.io.FileNotFoundException => status.error(name+": "+conf+" was not found ("+new java.io.File(conf).getAbsolutePath+")")
	        case x : java.io.IOException => status.error(name+": could not read "+conf)
	        case x => status.error(name+": parse exception "+pe)
        }
        case x => status.error(name+": error reading "+conf)
      }
      server
  }
  
  protected def initInstances() = for((k, v) <- instances.map){
    if(instancesMapToLoad!=null) instancesMapToLoad.get(k) match {
	  	  case None => {
println("found none for '"+k+"', creating server... ") 	  	  	
	  	    val ret = newServer(k,v)
	  	    val typename = if(ret!=null) ret.getClass.getName else "unknown type"
println("...created "+ret + " which is "+typename) 	  	  	

	  	    ret
	  	  } 
	  	  case Some(null) => {
println("found Some(null) for '"+k+"', creating server... ") 	  	  	
	  	    val ret = newServer(k,v)
println("...created "+ret) 	  	  	
	  	    ret
	  	  }
	  	  case Some(existing) => {
	  		  if(existing.conf.file != v){
println("found "+existing+" for '"+k+"', sutting down old ... ") 	  	  	
	  		    existing.shutdown
println("...and creating new...") 	  	  	
		  	    val ret = newServer(k,v)
println("...created "+ret) 	  	  	
		  	    ret
	  		  }
	      }   
	  	}
    }
}

object Instances {
  val nameToInstance = new mutable.HashMap[String, core.Server]()
  lazy val configuration = {
//new Exception("lazy val creation stack:").printStackTrace    
    new Instances(nameToInstance)
  }
}