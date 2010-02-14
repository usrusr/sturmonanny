package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema

class Configuration(override val file : String) extends ConfigurationSchema(file){   
	 
	object server  extends Group{   
	  object host extends Field( "127.0.0.1")
	  object il2port extends Field(2001)   	   
	  object consoleport extends Field(2011)
      object toolName extends Field("Sturmonanny")
	  object serverName extends Field("testserver") 
	  object pollMillis extends Field(1000) with Doc {
	    def doc = "SC pilots listing is too slow to be useful, set (minimum) number of milliseconds to pass between polls"
	  }
	  object serverPath extends Field("..") with Doc {
	    def doc = "path to the IL-2 server directory (used e.g. to access the i18n files)"
	  } 
	}
	object game extends Group { 

   
	  object planeWarningsSeconds extends Field(20) with Doc { def doc = """ time before a pilot gets kicked for flying a forbidden plane """ }
   
	  object warningInterval extends Field(2) with Doc { def doc = """ polling interval for the internal "user *" polling 
(SC constantly polls "user * STAT" but only rarely polls "user *" which is the only way to see _which_ plane he is flying) """
	  }


	  object startcost extends Field(10) with Doc {
	    def doc = """ how many minutes worth of flight time are debited as start fee for expensive planes """
	  }
   
	  object refund extends Field(50) with Doc {
	    def doc = """ how much of the starting fee of an expensive plane will get refunded when the pilots lands it in one piece
 examples:
  0:   no refund
  100: full refund
  50:  half refund """
	  }
    }
	object recruiting extends Group with Doc {
	  def doc = """ for a limited time after starting, pilots can invite (recruit) other pilots to fly the same plane
 recruited pilots are allowed to fly before their death-pause runs out 
****** NOT YET IMPLEMENTED *******
"""
	  object time extends Field(120) {
	    def doc = """ this number defines the seconds after start that players are allowed to recruit others into their group
****** NOT YET IMPLEMENTED *******
"""
	  }

	  object recruitshare extends Field(50) {
	    def doc = """ recruiting pilots have to pay a part of the starting fee for their recruits
****** NOT YET IMPLEMENTED *******
  0:   recruit pays all
  100: recruiter pays all
  50:  50:50 """
	  }
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


