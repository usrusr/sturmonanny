package de.immaterialien.sturmonanny.global

import de.immaterialien.sturmonanny.util.Logging
import de.immaterialien.sturmonanny.util.configgy

class GlobalConfig extends configgy.ConfigurationSchema("global.conf") with configgy.LiftSupport {
  doc = "some configuration that applies to the whole Sturmonanny installation"
	object admin extends Group{
	  doc = "login data for the web configuration interface"
	  object user extends Field("") // no internal default! 
	  object pass extends Field("") {doc = "set this for creating a new password, if the configuration is written by the software it will remain empty, only the hash is stored"}
	  object hash extends Field("") {doc = """never change this manually, set the "pass" value to change the password"""}
	}  
//	object instances extends Table("default.conf") { 
//	  doc = "list paths to configuration files defining the various sturmonanny instances running in this JVM" 
//	}
//	println("created globalConfig: "+new Exception().getStackTraceString)
}
