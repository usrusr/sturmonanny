package de.immaterialien.sturmonanny.fbdjhosting;

import de.immaterialien.sturmonanny.core.UpdatingMember
import de.immaterialien.sturmonanny.util.Logging

class FbdjAdapter extends UpdatingMember with Logging {
  private var fbdjPath = ""
  private var confPath = ""
  private var overridesPath = ""
  private var stats = false
  private var headless = true
  private var minutesPerMission = 0
  private var dcgCommand = ""
  private var autoconnect = false
  private var dcgPath = ""
  
  private var minSortiesBigger = 0
  private var minSortiesSmaller = 0
  private var minPilotsBigger = 0
  private var minPilotsSmaller = 0
  
  
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
     || conf.fbdj.autoconnect.apply != autoconnect
     || conf.fbdj.DCG.minutesPerMission.apply != minutesPerMission
     || conf.fbdj.DCG.dcgCommand.apply != dcgCommand
     || conf.fbdj.DCG.dcgPath.apply != dcgPath

     || conf.fbdj.DCG.dcgPath.apply != dcgPath
     ||   conf.fbdj.DCG.campaignProgress.minSorties.bigger.apply != minSortiesBigger
     ||   conf.fbdj.DCG.campaignProgress.minSorties.smaller.apply != minSortiesSmaller
     ||   conf.fbdj.DCG.campaignProgress.minPilots.bigger.apply != minPilotsBigger
     ||   conf.fbdj.DCG.campaignProgress.minPilots.smaller.apply != minPilotsSmaller
     
     
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
		  		autoconnect=conf.fbdj.stats
		  		minutesPerMission=conf.fbdj.DCG.minutesPerMission 
		  		dcgCommand=conf.fbdj.DCG.dcgCommand 
		  		dcgPath=conf.fbdj.DCG.dcgPath 
		  		minSortiesBigger=conf.fbdj.DCG.campaignProgress.minSorties.bigger
		  		minSortiesSmaller=conf.fbdj.DCG.campaignProgress.minSorties.smaller
		  		minPilotsBigger=conf.fbdj.DCG.campaignProgress.minPilots.bigger
		  		minPilotsSmaller=conf.fbdj.DCG.campaignProgress.minPilots.smaller      
      
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
