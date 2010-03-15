package de.immaterialien.sturmonanny.global

import de.immaterialien.sturmonanny.util.Logging
import de.immaterialien.sturmonanny.util.configgy
import scala.collection.mutable
import scala.collection.immutable
import de.immaterialien.sturmonanny.core 

class Instances extends configgy.ConfigurationSchema("instances.conf") with configgy.LiftSupport { 
  doc = """list of the internal sturmonanny instances"""
	object instances extends Table("default.conf") { 
	  doc = """list paths to configuration files defining the various sturmonanny instances running in this JVM
example: 
server1 = server1.conf
server2 = server2.conf 
"""
	}
  override def readConfiggy(in:net.lag.configgy.Config)={
    super.readConfiggy(in)
println("instances read: "+this+"\n---from---"+in)
    Instances.nameToInstance.retain((k,v)=>{
println("found in map: "+k+" is "+v)
      if(instances.map.keys contains k){
println("keeping "+k)
        true
      }else{
println("dropping "+k)
        Instances.nameToInstance(k).shutdown        
        false
      }
    })
    def newServer(name:String, conf:String):Unit={
      try{ 
        val server = core.Server.withThreadGroup(conf)
println("created server for "+name+": "+server)        
        Instances.nameToInstance.put(name, server)
      }catch {
        case x : java.io.FileNotFoundException => status.error("file "+conf+" for "+name+" was not found ("+new java.io.File(conf).getAbsolutePath+")")
        case x : java.io.IOException => status.error("could not read "+conf+" for "+name)
        case x => status.error("error reading "+conf+" for "+name+": "+x)
      }
    }
    for((k, v) <- instances.map){
      Instances.nameToInstance.get(k) match {
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
}

object Instances {
  var nameToInstance = new mutable.HashMap[String, core.Server]()
  val configuration = new Instances()
}