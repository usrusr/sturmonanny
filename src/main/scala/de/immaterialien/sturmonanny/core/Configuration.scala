package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util.configgy

class Configuration(override val file : String) extends configgy.ConfigurationSchema(file) with configgy.LiftSupport{   
	doc = "configuration for a single host instance"
	object server  extends Group{    
	  object host extends Field( "127.0.0.1")
	  object il2port extends Field(2001)   	   
	  object consoleport extends Field(2011)
      
	  object serverName extends Field("testserver") 
	  object pollMillis extends Field(1000)  {
	    doc = "SC pilots listing is too slow to be useful, set (minimum) number of milliseconds to pass between polls"
	  }
	  object serverPath extends Field("..")  {
	    doc = "path to the IL-2 server directory (used e.g. to access the i18n files)"
	  } 
	}
	object game extends Group { 
		object homeAlone extends Field(false) { doc = """disables most sturmonanny features, 
use this to host a plain DCG-enabled FBDj session within sturmonanny without aircraft cost, recruiting
"""
		}
   
	  object planeWarningsSeconds extends Field(20)  { doc = """ time before a pilot gets kicked for flying a forbidden plane """ }
   
	  object warningInterval extends Field(2)  { doc = """ polling interval for the internal "user *" polling 
(SC constantly polls "user * STAT" but only rarely polls "user *" which is the only way to see _which_ plane he is flying) """
	  }


	  object startcost extends Field(10)  {
	    doc = """ how many minutes worth of flight time are debited as start fee for expensive planes """
	  }
   
	  object refund extends Field(50)  {
	    doc = """ how much of the starting fee of an expensive plane will get refunded when the pilots lands it in one piece
 examples:
  0:   no refund
  100: full refund
  50:  half refund """
	  }
    }
	object recruiting extends Group  {
	  doc = """ for a limited time after starting, pilots can invite (recruit) other pilots to fly the same plane
 recruited pilots are allowed to fly before their death-pause runs out 
****** NOT YET IMPLEMENTED *******
"""
	  object time extends Field(120) {
	    doc = """ this number defines the seconds after start that players are allowed to recruit others into their group
"""
	  }

	  object recruitshare extends Field(50) {
	    doc = """ recruiting pilots have to pay a part of the starting fee for their recruits
  0:   recruit pays all
  100: recruiter pays all
  50:  50:50 
landing refund should be distributed in the same ratio but this is not implemented yet
"""
	  }
	}
	object pilots extends Group {
	  object highestBalance extends Field(1000)
	  object lowestBalance extends Field(0)
	  object startBalance extends Field(10)
   
   	  object deathpenalty extends Field(60)  {
   	    doc = """ seconds of death penalty, pilots will not be allowed to board a plane during this time"""
   	  }
	}

	object market extends Group {
	  object implementation extends Field("de.immaterialien.sturmonanny.core.AllPlanesEqualMarket")
	  object configuration extends Field("planes.lst")
	} 
 
	object names extends Group {
	  doc = "a few strings you can set to customize messages"
	  object tool extends Field("Sturmonanny")
	  object currency extends Field("%s")
	} 
	object fbdj extends Group {
	  doc = "sturmonanny can host an FBDj internally, keep empty if you don't want this to happen"
	  object installationPath extends Field("")  {
		  doc = """a (relative) path to your FBDj installation, 
should contain FBDj.jar and all the other stuff
    
note: the FBDj.jar must not be on your regular classpath for sturmonanny"""
	  }
	  object overridesJar extends Field("FBDj-overrides.jar")  {
doc = """a (relative) path to your FBDj-overrides jar,
which is required for FBDj embedding"""	    
	  }
	  object fbdjConfiguration extends Field("fbdjConf.ser")  {
		  doc = """path to the FBDj configuration for this instance
if the path with . it will be relative to the FBDj installation, 
otherwise it will be relative to sturmonanny installation (or absolute)
"""	    
	  }   

   object headless extends Field(false){
	   doc = """set true to keep the FBDj window from appearing (default: false)"""	       
   }
   
   object stats extends Field(true){
	   doc = """start FBDj with the stats automatically started (default: true, true overrides FBDj autostats)"""	       
   }
   object autoconnect extends Field(true){
	   doc = """start FBDj with the stats automatically started 
(default: true, 
always true when headless, 
true overrides FBDj autoconnect, 
FBDj Auto Start setting might be working better
)"""
    
   }
   
   object DCG extends Group {
     doc = "configure the DCG compatibility addon for FBDj (using the SC mode of DCG, not the FBD mode)"
     object minutesPerMission extends Field(60){
       doc="duration of a single DCG mission, the FBDj mission will run a little longer, waiting for DCG to create the next mission"
     }

     object dcgCommand extends Field("il2dcg.exe /netdogfight") {
       doc="""Command line to execute for creating the next SC mission, must not return before the mission is created. 
After the command returns the latest new .mis file from the mission directory of the current mission will be started by FBDj
example: "C:\myDcgInstallation\il2dcg.exe /netdogfight" """
       
     }
     object dcgPath extends Field("") {
       doc="""path to the DCG installation, 
required to localize the DCG.ini (if empty, FBDj will assume that the DCG.ini resides in the mission directory)"""
       
     }
     
     object campaignProgress extends Group{
   	  doc = """here you can define some criteria that have to be met for campaign progress, 
if they are not met the same map will be repeated over and over again.
a minimum value for the "bigger" side means that at least one army has to reach the minimum, 
a minimum value for the "smaller" side means that both armies have to reach the minimum to make the DCG campaign progress. 
(default values are all 0 wich means that the campaign will even progress if no human pilots are flying)"""
      object minSorties extends Group {
        doc="""counts total sorties, use this instead of minPilots to enable single pilots to progress the campaign"""
        object bigger extends Field(0)
        object smaller extends Field(0)
      }
      object minPilots extends Group {
        doc="""only counts individual pilots, use this instead of minSorties to keep single pilots from progressing the campaign alone"""
        object bigger extends Field(0)
        object smaller extends Field(0)
      }
     }

     
     
   }
   
	}
}
object Configuration {
   object Default extends Configuration("default.conf")
}


