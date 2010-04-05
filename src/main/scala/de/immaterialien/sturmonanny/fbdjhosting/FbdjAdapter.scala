package de.immaterialien.sturmonanny.fbdjhosting;

import de.immaterialien.sturmonanny.core.UpdatingMember
import de.immaterialien.sturmonanny.util.Logging

class FbdjAdapter extends UpdatingMember with Logging {
  private var fbdjPath = ""
  private var confPath = ""
  private var overridesPath = ""
  private var stats = true
  private var headless = true
  private var minutesPerMission = 0
  private var dcgCommand = ""
  
  
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
     || conf.fbdj.headless.apply != headless
     || conf.fbdj.stats.apply != stats
     || conf.fbdj.DCG.minutesPerMission.apply != minutesPerMission
     || conf.fbdj.DCG.dcgCommand.apply != dcgCommand
     || ! fbdj.isDefined
	  	)
	  ) {
debug("FBDj configuratoin changing!")	  
		  	try{
		  		val created = new FbdjHost(conf)
		  		fbdj = Some(created)
		  		fbdjPath = conf.fbdj.installationPath
		  		confPath = conf.fbdj.fbdjConfiguration
		  		headless=conf.fbdj.headless
		  		stats=conf.fbdj.stats
		  		minutesPerMission=conf.fbdj.DCG.minutesPerMission 
		  		dcgCommand=conf.fbdj.DCG.dcgCommand 
      
      
		  		overridesPath = conf.fbdj.overridesJar
	  	    debug("initialized FBDj: \n  "+fbdjPath+"\n  "+overridesPath+"\n  "+confPath+"\n   in:"+System.identityHashCode(fbdj.get.inList)+"   out:"+System.identityHashCode(fbdj.get.outList))
		  		server.multi.internalConnection ! server.multi.internalConnection.UpdatedQueues 
		  	}catch{
		  	  case x => {
		  	    debug("failed to setup FBDj: "+x)
		  	  }
		  	}
	    
	  	}
	}
  

  
 
}
