package de.immaterialien.sturmonanny.snippet

import scala.collection.mutable
import scala.xml._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util.NamedPF
import net.liftweb.http.LiftRules
 
import de.immaterialien.sturmonanny.util.configgy._
import de.immaterialien.sturmonanny.global._

 

class InstanceConf {

} 
 
 
object InstanceConf {
  def registerRules{
	  LiftRules.statelessRewrite.prepend(NamedPF("InstanceConf") {
    	case RewriteRequest(ParsePath("instance" :: instanceName :: "conf" :: Nil, _, _,_), _, _) => 
    		RewriteResponse(
    				"instance/name/conf" :: Nil, Map("name" -> instanceName)  // Use webapp/instance/name/conf.html
    		)
	  })
  }
  def confLink(name:String):String={
    "instance/"+java.net.URLEncoder.encode(name)+"/conf"
  }
}