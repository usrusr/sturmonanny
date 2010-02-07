package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema

class Configuration(override val file : String) extends ConfigurationSchema(file){   
	 
	object server  extends Group{   
	  object host extends Field( "127.0.0.1")
	  object il2port extends Field(2001)   	  
	  object consoleport extends Field(2011)
      object toolName extends Field("Sturmonanny")
	  object serverName extends Field("testserver")
	}
	object game extends Group {
	  object deathpenalty extends Field(2)
	  object warningsToKick extends Field(10)
	  object warningInterval extends Field(3)
	  object startcost extends Field(10)
	  object refund extends Field(50)
	  object accountlimit extends Field(1000)
	  object recruitshare extends Field(50)
 
	}

	object market extends Group {
	  object implementation extends Field("de.immaterialien.sturmonanny.core.AllPlanesEqualMarket")
	  object configuration extends Field("planes.lst")
	} 
}
object Configuration {
   object Default extends Configuration("default.conf")
}


