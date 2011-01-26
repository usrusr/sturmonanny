package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util.configgy
import _root_.de.immaterialien.sturmonanny

class Configuration(override val file: String, val serverInstance: sturmonanny.core.Server) extends configgy.ConfigurationSchema(file) with configgy.LiftSupport {

  override def apply(conf: net.lag.configgy.Config): Option[java.io.File] = {
    val ret = super.apply(conf)

    if (serverInstance != null) serverInstance.conf = this

    ret
  }

  doc = "configuration for a single host instance"
  object server extends Group {
    object host extends Field("127.0.0.1") { doc = "host for the console connection" }
    object il2port extends Field(2001) {
      doc = """console port of the IL2-server that should be connected
if your confs.ini looks like this:
...
[Console]
IP=123
...
then your il2port is 123
"""
    }
    object consoleport extends Field(2011) {
      doc = """exposed console port of sturmonanny instance, chose an otherwise unused port (or null, to avoid any console connections)
(this can be used to connect an external tool like IL2 SC or a manual console like il2wconsole.exe, the extneral console 
client will not see the console traffic between sturmonanny and the il2-server, which includes any traffic created by an embedded FBDj)
"""
    }
    object IPS extends Field("127.0.0.1, 0:0:0:0:0:0:0:1") {
      doc = """comma-separated IP whitelist for the console interface, 
works like the IPS item in the [Console] section of confs.ini"""
    }

    //	  object serverName extends Field("testserver") 
    object pollMillis extends Field(1000) {
      doc = "SC pilots listing is too slow to be useful, set (minimum) number of milliseconds to pass between polls"
    }
    object serverPath extends Field("..") {
      doc = "path to the IL-2 server directory (used e.g. to access the i18n files)"
    }
  }
  object game extends Group {
    object homeAlone extends Field(false) {
      doc = """disables most sturmonanny features, 
use this to host a plain DCG-enabled FBDj session within sturmonanny without aircraft cost, recruiting
"""
    }

    object planeWarningsSeconds extends Field(20) { doc = """ time before a pilot gets kicked for flying a forbidden plane """ }

    object warningInterval extends Field(2) {
      doc = """ polling interval for the internal "user *" polling 
(SC constantly polls "user * STAT" but only rarely polls "user *" which is the only way to see _which_ plane he is flying) """
    }

    object startcost extends Field(10) {
      doc = """ how many minutes worth of flight time are debited as start fee for expensive planes """
    }

    object refund extends Field(50) {
      doc = """ how much of the starting fee of an expensive plane will get refunded when the pilots lands it in one piece
 examples:
  0:   no refund
  100: full refund
  50:  half refund """
    }
	  object kickCommand extends Field("kick $qname") {
	    doc = """ customize the kick behavior: what sturmonanny will do when it thinks a player should be kicked
variables: 
	    	$name   : will be replaced by the player name
	    	$qname  : will be replaced by the quoted player name
	    	$reason : kick reason, might be empty
control characters: 
	    	\n    : separates two commands, 
                e.g. "kick $qname\nchat $name kicked for $reason TO ALL" 
                     (kick and inform the public)
                e.g. "chat $name not kicked for testing purposes TO ALL\nchat you would have been kicked for $reason TO $qname"
                     (test mode, inform but don't kick)	    	
	    	\n(x) : x has to be a number, representing the number of seconds to wait between commands,
	    					e.g. "chat you will be kicked in 5s for $reason to $qname\n(5)kick $qname\nchat $qname was kicked for $reason TO ALL"
                     (give the kicked player a chance to read the reason before feeling the boot)
"""
	  }    
  }


  object recruiting extends Group {
    doc = """ for a limited time after starting, pilots can invite (recruit) other pilots to fly the same plane
 recruited pilots are allowed to fly before their death-pause runs out 
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
    object highestBalance extends Field(1000) {
      doc = """upper limit for the amount of "cash" a pilot can stockpile, everything gains above this will be ignored"""
    }
    object lowestBalance extends Field(-500) {
      doc = """lower limit for the amount of "cash" a pilot can lose, payments below this limit are ignored"""
    }
    //	  object startBalance extends Field(10)

    object deathpenalty extends Field(90) {
      doc = """ seconds of death penalty, pilots will not be allowed to board a plane during this time (total upper limit)"""
    }
    
    object penaltyPerPilot extends Field(10) {
      doc = """ relative death penalty: number of seconds penalty grows for each player on the same team 
(the absolute limit is set by deathpenalty, if penaltyPerPilot is larger than penaltyPerPilot even solo players will get penalty
otherwise a solo player would be able to refly at once)"""
    }
    
  }

  object market extends Group {
    object implementation extends Field("de.immaterialien.sturmonanny.core.AllPlanesEqualMarket")
    object configuration extends Field("planes.lst")
  }
  object persistence extends Group {
    doc = "pluggable storage for pilot balance state, e.g. connecting to a database, flat text file or simply storing/forgetting in RAM"
    object implementation extends Field("de.immaterialien.sturmonanny.persistence.InMemoryBackend") {
      doc = "fully qualified class name of a class implementing trait de.immaterialien.sturmonanny.persistence.IBalanceDao"
    }
    object properties extends Table("") {
      doc = "configuration properties for the IBalanceDao implementation, e.g. filename, JDBC driver, URL and credentials or something similar"
    }
    object jdbc extends Group {
    	doc="the usual quad for jdbc database connections"
    	object driver extends Field("org.h2.Driver")
			object url extends Field("jdbc:h2:nannydb;MODE=MySQL")
			object user extends Field("SA")
			object pass extends Field("")
    }
  }

  object names extends Group {
    doc = "a few strings you can set to customize messages"
    object tool extends Field("Sturmonanny")
    object currency extends Field("%s")
  }
  object fbdj extends Group {
    doc = """sturmonanny can host an FBDj internally, keep empty if you don't want this to happen

"""
    object installationPath extends Field("") {
      doc = """a (relative) path to your FBDj installation, 
should contain FBDj.jar and all the other stuff
    
note: the FBDj.jar must not be on your regular classpath for sturmonanny"""
    }
    object overridesJar extends Field("FBDj-overrides.jar") {
      doc = """a (relative) path to your FBDj-overrides jar,
which is required for FBDj embedding.

note: The version of the FBDj installation referenced above has to be exactly the version the FBDj-overrides jar was made for 
(at time of this writing: 1.5b)
"""
    }
    object fbdjConfigurationDirectory extends Field("./config/Default") {
      doc = """path to the FBDj configuration for this instance
if the path starts with . it will be relative to the FBDj installation, 
otherwise it will be relative to sturmonanny installation (or absolute)
"""
    }

    object headless extends Field(false) {
      doc = """set true to keep the FBDj window from appearing (default: false, only set to true if you know exactly that the configuration will do what you want)"""
    }

    //   object stats extends Field(true){
    //	   doc = """start FBDj with the stats automatically started (default: true, true overrides FBDj autostats)"""	       
    //   }
    object autoconnect extends Field(true) {
      doc = """start FBDj with the stats automatically started 
(default: true, 
always true when headless, 
overrides FBDj autoconnect 
)"""

    }

    object DCG extends Group {
      doc = "configure the DCG compatibility addon for FBDj (using the SC mode of DCG, not the FBD mode)"
      // ignored, uses DCG setting   
      //     object minutesPerMission extends Field(60){
      //       doc="duration of a single DCG mission, the FBDj mission will run a little longer, waiting for DCG to create the next mission"
      //     }
      object dcgPath extends Field("C:/DCG") {
        doc = """path to the DCG installation, 
required to find the DCG.ini (if empty, FBDj may assume that the DCG.ini resides in the mission directory
 - this can only work if the dcgCommand uses an absolute path or something else that works from the mission directory, 
and may need additional DCG files to reside there)"""

      }
      object dcgCommand extends Field("il2dcg.exe /netdogfight") {
        doc = """Command line to execute for creating the next SC mission, must not return before the mission is created. 
After the command returns the latest new .mis file from the mission directory of the current mission will be started by FBDj.
Will be started in the directory defined in dcgPath.
example: "il2dcg.exe /netdogfight" """

      }


      object campaignProgress extends Group {
        doc = """here you can define some criteria that have to be met for campaign progress, 
if they are not met the same map will be repeated over and over again.
a minimum value for the "bigger" side means that at least one army has to reach the minimum, 
a minimum value for the "smaller" side means that both armies have to reach the minimum to make the DCG campaign progress. 
(default values are all 0 wich means that the campaign will even progress if no human pilots are flying)
****** currently not implemented correctly, the fdbj mod does not recognise the correct side of a sortie fast enough ******* 
"""
        object minSorties extends Group {
          doc = """counts total sorties, use this instead of minPilots to enable single pilots to progress the campaign"""
          object bigger extends Field(0)
          object smaller extends Field(0)
        }
        object minPilots extends Group {
          doc = """only counts individual pilots, use this instead of minSorties to keep single pilots from progressing the campaign alone"""
          object bigger extends Field(0)
          object smaller extends Field(0)
        }
      }

      object addons extends Table(0) {
        doc = """optional pre- or postprocessing for the mission generator script inside of the JVM

purpose: while processing with command-line tools can easily be included in a script referenced by the dcgCommand parameter, 
processing implemented in a JVM-language (e.g. java) would suffer from JVM startup times if set up that way. To solve this 
JVM-processing should better be invoked inside the sturmonanny JVM.

example configuration:
<addons>
my_package_Preprocessor = -10
my_package_Postprocessor = 1
my_other_package_AnotherPostprocessor = 10
</addons>

Keys (left of the equals sign) have to be fully qualified class names of classes present on the classpath 
(with the dots replaced by underscores, as the config format does not allow dots - if you really have to 
use an underscore you can double-escape it: "my_stupid_package.The_class" would become "my__stupid__package_The__class"). 
Values (right of the equals sign) have to be unique numbers: 
	* negative values for preprocessors (called before the script invokation)
  * positive values for postprocessors (called after the script invokation)
The exact value of the numbers defines the order in which the processors are called, ascending from lowest negative 
value to highest positive values, with the DCG console command at position 0 

Implementation notes: the processor classes have to implement javax.xml.ws.Provider<java.io.File> 
(used as a simple File -> File filter interface). 
The processors form a filter chain: the invoke method of the first processor 
(a preprocessor or the implicit CLI script at position 0) is called with a File 
object pointing at the current .mis file, all further processors are called with 
the result of the last processor invoke call. In case of a thrown exception or an 
invalid return value the last input is used. Processor objects are either created 
from the default constructor or from a single String argument constructor, if available. 
Use the <addonArguments> configuration for setup through this String argument constructor.  
"""
      }
      object addonArguments extends Table("") {
        doc = """optional configuration for processors defined in <addons>, same escaping of qualified class names as in <addons>
 
example configuration:
<addonArguments>
my_package_Postprocessor = "this will be used as argument for a String argument constructor of my.package.Postprocessor, if available"
</addonArguments>
"""

      }
    }

  }
}
object Configuration {
  def default(serverInstance: Server) = new Configuration("default.conf", serverInstance)
}

