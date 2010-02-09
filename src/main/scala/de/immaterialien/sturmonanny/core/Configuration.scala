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

   
	  private object docWarnzeit extends Documentation(""" time before a pilot gets kicked for flying a forbidden plane """)
	  object planeWarningsSeconds extends Field(20)
   
	  private object docWarnPeriode extends Documentation(""" polling interval for the internal "user *" polling 
(SC constantly polls "user * STAT" but only rarely polls "user *" which is the only way to see _which_ plane he is flying) """)
	  object warningInterval extends Field(2)

	  private object docStartkosten extends Documentation(""" polling interval for the internal "user *" polling 
(SC constantly polls "user * STAT" but only rarely polls "user *" which is the only way to see _which_ plane he is flying) """)
   
   
	  object startcost extends Field(10) with Doc {
	    def doc = """ polling interval for the internal "user *" polling 
(SC constantly polls "user * STAT" but only rarely polls "user *" which is the only way to see _which_ plane he is flying) """
	  }
   
	  object refund extends Field(50)
	  object recruitshare extends Field(50)
	}
	object pilots extends Group {
	  object highestBalance extends Field(1000)
	  object lowestBalance extends Field(0)
	  object startBalance extends Field(10)
   
   	  object deathpenalty extends Field(60) with Doc {
   	    def doc = """ seconds of death penalty, pilots will not be allowed to board a plane during this time"""
   	  }
	}

	object market extends Group {
	  object implementation extends Field("de.immaterialien.sturmonanny.core.AllPlanesEqualMarket")
	  object configuration extends Field("planes.lst")
	} 
}
object Configuration {
   object Default extends Configuration("default.conf")
}


