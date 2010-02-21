package de.immaterialien.sturmonanny.fbdjhosting;

import de.immaterialien.sturmonanny.core.UpdatingMember
import de.immaterialien.sturmonanny.util.Logging

class FbdjAdapter extends UpdatingMember with Logging {
  private var fbdjPath = ""
  private var confPath = ""
  private var overridesPath = ""
  
  var fbdj : Option[FbdjHost] = None
  
	def updateConfiguration {
debug("beginning to set up FBDj at '"+conf.fbdj.installationPath.apply+"'")	  
	  if( 
		   ( ! conf.fbdj.installationPath.apply.isEmpty) 
			  &&	
     (	  
			  conf.fbdj.installationPath.apply!=fbdjPath
     || conf.fbdj.fbdjConfiguration.apply!=confPath
     ||	conf.fbdj.overridesJar.apply!=overridesPath
     || ! fbdj.isDefined
	  	)
	  ) {
debug("FBDj configuratoin changing!")	  
		  	try{
		  		val created = new FbdjHost(conf.fbdj.installationPath, conf.fbdj.overridesJar, conf.fbdj.fbdjConfiguration.apply)
		  		fbdj = Some(created)
		  		fbdjPath = conf.fbdj.installationPath
		  		confPath = conf.fbdj.fbdjConfiguration
		  		overridesPath = conf.fbdj.overridesJar
	  	    debug("initialized FBDj: \n  "+fbdjPath+"\n   "+overridesPath+"\n   "+confPath)
		  	}catch{
		  	  case x => {
		  	    debug("failed to setup FBDj: "+x)
		  	  }
		  	}
	    
	  	}
	}
  

  
 
}
