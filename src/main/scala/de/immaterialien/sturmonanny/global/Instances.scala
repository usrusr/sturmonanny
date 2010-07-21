package de.immaterialien.sturmonanny.global

import de.immaterialien.sturmonanny.util.Logging
import de.immaterialien.sturmonanny.util.configgy
import scala.collection.mutable
import scala.collection.immutable
import de.immaterialien.sturmonanny.core 

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
  private def newServer(name:String, conf:String):Unit={
      try{ 
        val server = core.Server.withThreadGroup(conf)
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
  }
  
  protected def initInstances() = for((k, v) <- instances.map){
    if(instancesMapToLoad!=null) instancesMapToLoad.get(k) match {
	  	  case None => {
	  	    newServer(k,v)
	  	  } 
	  	  case Some(null) => {
	  	    newServer(k,v)
	  	  }
	  	  case Some(existing) => {
	  		  if(existing.initConf != v){
	  		    existing.shutdown
	  		    newServer(k,v)
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