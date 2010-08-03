package de.immaterialien.sturmonanny.fbdjhosting;

import _root_.de.immaterialien.sturmonanny.core
import _root_.de.immaterialien.sturmonanny.util.Logging
import _root_.de.immaterialien.sturmonanny.util.event

class FbdjAdapter extends core.UpdatingMember with Logging {
  private var fbdjPath = "."
  private var confPath = ""
  private var overridesPath = ""
//  private var stats = false
  private var headless = true
//  private var minutesPerMission = 0
  private var dcgCommand = ""
  private var autoconnect = false
  private var dcgPath = ""
  
  private var minSortiesBigger = 0
  private var minSortiesSmaller = 0
  private var minPilotsBigger = 0
  private var minPilotsSmaller = 0
  
  private var addons = Map[String, Int]()
  private var addonArguments = Map[String, String]() 
  private var nextMissionProvider : NextMissionProvider = null
  
  var fbdj : Option[FbdjContainer] = None
  
	def updateConfiguration = {
	  if( ! fbdj.isDefined ) newHost
	  if(registration.isDefined) ()//newHost
    else registration = Some(server.multi.il2ConnectionNotifier subscribe {connect =>
debug("FBDJ connect "+connect+" with "+fbdj.isDefined)
			if( ! fbdj.isDefined) newHost
      if(connect){
        
        fbdj foreach (_.connect(connect))
      }else{
        fbdj foreach (_.connect(connect))
      }
//      fbdj = None
//      if(connect) newHost
    })
  } 
	
	var registration : Option[event.Subscription] = None
 
	def newHost {
debug("beginning to set up FBDj at '"+conf.fbdj.installationPath.apply+"'")	  
	  if( 
		   ( ! conf.fbdj.installationPath.apply.isEmpty) 
			  &&	
     (	  
			  conf.fbdj.installationPath.apply!=fbdjPath
     || conf.fbdj.fbdjConfigurationDirectory.apply!=confPath
     ||	conf.fbdj.overridesJar.apply!=overridesPath
     || conf.fbdj.headless.apply != headless
//     || conf.fbdj.stats.apply != stats 
     || conf.fbdj.autoconnect.apply != autoconnect
//     || conf.fbdj.DCG.minutesPerMission.apply != minutesPerMission
     || conf.fbdj.DCG.dcgCommand.apply != dcgCommand
     || conf.fbdj.DCG.dcgPath.apply != dcgPath

     || conf.fbdj.DCG.dcgPath.apply != dcgPath
     ||   conf.fbdj.DCG.campaignProgress.minSorties.bigger.apply != minSortiesBigger
     ||   conf.fbdj.DCG.campaignProgress.minSorties.smaller.apply != minSortiesSmaller
     ||   conf.fbdj.DCG.campaignProgress.minPilots.bigger.apply != minPilotsBigger
     ||   conf.fbdj.DCG.campaignProgress.minPilots.smaller.apply != minPilotsSmaller
     
     ||   conf.fbdj.DCG.addons.map != addons
     ||   conf.fbdj.DCG.addonArguments.map != addonArguments
     
     || ! fbdj.isDefined
	  	)
	  ) {
debug("FBDj configuratoin changing!")	  
		  	try{

		  	  
		  		//val created = new FbdjHost(conf)
		  	  
		  	  val created = ContainerPool.getContainer(conf.fbdj.installationPath.apply, conf.fbdj.overridesJar.apply)
		  	  
		  	  created.changeConfiguration(conf, server.initConf)
		  	  created.start  
       
       
		  		fbdj = Some(created)
		  		fbdjPath = conf.fbdj.installationPath.apply
		  		confPath = conf.fbdj.fbdjConfigurationDirectory.apply
		  		headless=conf.fbdj.headless.apply
//		  		stats=conf.fbdj.stats
//		  		autoconnect=conf.fbdj.stats
//		  		minutesPerMission=conf.fbdj.DCG.minutesPerMission 
		  		dcgCommand=conf.fbdj.DCG.dcgCommand .apply
		  		dcgPath=conf.fbdj.DCG.dcgPath .apply
		  		minSortiesBigger=conf.fbdj.DCG.campaignProgress.minSorties.bigger.apply
		  		minSortiesSmaller=conf.fbdj.DCG.campaignProgress.minSorties.smaller.apply
		  		minPilotsBigger=conf.fbdj.DCG.campaignProgress.minPilots.bigger.apply
		  		minPilotsSmaller=conf.fbdj.DCG.campaignProgress.minPilots.smaller.apply  
      
          addons = conf.fbdj.DCG.addons.map 
          addonArguments = conf.fbdj.DCG.addonArguments.map
      
          
          nextMissionProvider = new NextMissionProvider(conf) 
          
		  		overridesPath = conf.fbdj.overridesJar.apply
	  	    debug("initialized FBDj: \n  "+fbdjPath+"\n  "+overridesPath+"\n  "+confPath+"\n   in:"+System.identityHashCode(fbdj.get.inList)+"   out:"+System.identityHashCode(fbdj.get.outList))
		  		server.multi.internalConnection ! server.multi.internalConnection.UpdatedQueues 
		  		server.multi.eventLogConnection ! server.multi.eventLogConnection.UpdatedQueues 
		  	}catch{
		  	  case x => {
		  	    debug("failed to setup FBDj: "+x)
		  	  }
		  	}
	    
	  	}else{
	  		debug("no reason found to set up FBDj : \n  "+fbdjPath+"\n  "+overridesPath+"\n  "+confPath+"\n   in:"+System.identityHashCode(fbdj.get.inList)+"   out:"+System.identityHashCode(fbdj.get.outList))
	  	}
   		if(conf.fbdj.autoconnect.apply != autoconnect){
   		  autoconnect = conf.fbdj.autoconnect.apply
//        fbdj.foreach(_ connect autoconnect)
   		}

	}
  

  
 
}
