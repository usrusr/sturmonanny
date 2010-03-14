package de.immaterialien.sturmonanny.global

import de.immaterialien.sturmonanny.util.Logging
import de.immaterialien.sturmonanny.util.configgy
 
class Instances extends configgy.ConfigurationSchema("instances.conf") with configgy.LiftSupport { 
  doc = """list of the internal sturmonanny instances"""
	object instances extends Table("default.conf") { 
	  doc = """list paths to configuration files defining the various sturmonanny instances running in this JVM
example: 
server1 = server1.conf
server2 = server2.conf
"""
	}  
}