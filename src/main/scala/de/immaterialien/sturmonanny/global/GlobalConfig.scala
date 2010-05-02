package de.immaterialien.sturmonanny.global

import de.immaterialien.sturmonanny.util.Logging
import de.immaterialien.sturmonanny.util.configgy
 
class GlobalConfig(private val fname:String) extends configgy.ConfigurationSchema(fname) with configgy.LiftSupport {
  def this()=this("global.conf")
  doc = "some configuration that applies to the whole Sturmonanny installation"

	object admin extends Group{
	  doc = "login data for the web configuration interface"
	  object user extends Field("") { // no internal default!
	    doc = "login name (case sensitive)"
      override def update(usr:String) = {
        hash() = GlobalConfig.hashPass(usr, pass)
        super.update(usr) 
      }	    
	  }
	  object pass extends Field("") {
	    doc = "set this for creating a new password, if the configuration is written by the software it will remain empty, only the hash is stored"
      override def update(pwd:String) = {
        hash() = GlobalConfig.hashPass(user, pwd)
      }
	  }
	  object hash extends Field("") {doc = """never change this manually, set the "pass" value to change the password"""}
	} 
  object jetty extends Group{
    object port extends Field(8080){
      doc="""port for the internal configuration web interface"""
    }
  }
  println("instances configuration loaded")
}
object GlobalConfig {
  lazy val singleton = new GlobalConfig
  private val messageDigestInstance = java.security.MessageDigest.getInstance("SHA-1")
  private val pwdSalt = "/=V"
  def hashPass(_user:String, _pass:String)={
    val user = if(_user==null) "" else _user.trim
    val pass = if(_pass==null) "" else _pass.trim
    if(user=="" && pass=="") ""
    else {
	    val digest = messageDigestInstance.digest((pwdSalt+user.trim.toUpperCase+pass.trim).toCharArray.map(_.toByte))
	    javax.xml.bind.DatatypeConverter.printBase64Binary(digest.take(8))
    }
  }
}