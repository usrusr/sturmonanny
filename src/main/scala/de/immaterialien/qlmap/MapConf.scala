package de.immaterialien.qlmap

import de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema
 
class MapConf(private val fname:String) extends ConfigurationSchema(fname) with MapConfiguration {
	override lazy val outPath = { import java.io.File
		val outPathString = reconDir.apply
		
		log.debug("reconDir:" + outPathString) 
    val out = if (outPathString startsWith ".") {
    	if(fileReference.isDefined){
    		new File(fileReference.get.getParent + File.separator + outPathString)
    	}else{
    		val f = new File(outPathString)
    		log.warning(outPathString + " is a relative path, but the MapConf is initialized from default values, current directory as base, getting "+f.getAbsoluteFile)
    		f
    	}
    } else new File(outPathString)
    
    if (out.isFile) {
    	log.warning(out.getAbsolutePath + " is a file, need a directory")
    	None
    } else if (out.exists || out.mkdirs) {
    	Some(out)
    } else {
    	log.warning("failed to create recon directory " + out.getAbsolutePath)
    	None
    }
	}
	object reconDir extends Field("."){
		doc="""configures the path where the created recon files will be written to
				   if it is relative then it will be interpreted as relative to mapbase """
	}
	object debug extends Field(false, "set to true for full chief route display")
	object front extends Group {
		object interpolate extends Field(5){
			doc="larger: faster, more jaggy lines (roughly: biggest jaggyness-error in pixels)"
		}
		object subdivisions extends Field(4, "")
		object aa extends Field(2, "antialiasing, higher: smother front line, slower")
		object hatchdistance extends Field(15, "distance between hatching lines")
	}
	object units extends Group {
		doc="static units"
		object groundRadius extends Field(5000, "how far do we look from a frontmarker to identify its size and type")
		object depth extends Field(100, "static visibility, in % of default static visibility depth (higher: deeper recon)")
	}
	object chiefs extends Group {
		object depth extends Field(100, "chief visibility, in % of default chief visibility depth (higher: deeper recon)")
		object moveThreshold extends Field(10, "lower: prefer arrow icon over battle icon")
	}
	object wings extends Group {
		object probabilities extends Group { doc = "probabilities, independent of depth"
			object groundAttack extends Field(0, "probability of a planned AI ground attack to be known")
		}
	}
}