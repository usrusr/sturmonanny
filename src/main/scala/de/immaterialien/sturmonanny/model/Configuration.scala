package de.immaterialien.sturmonanny.model

import _root_.net.lag.configgy
import _root_.de.immaterialien.sturmonanny.util.ConfiggyGroup
import _root_.de.immaterialien.sturmonanny.multiplexer

class Configuration(val name : String, val file : String) {
	object server  extends ConfiggyGroup{ 
	  var host : String = try{java.net.InetAddress.getLocalHost.getHostName}catch{case _ => "127.0.0.1"}
	  var il2port = 2001
	  var consoleport = 2011
	  override def update(conf : Reconfigure) {
	    host = conf get "host" or host
	    il2port = conf get "il2port" or il2port
	    consoleport = conf get "consoleport" or consoleport
	  } 
	}
	object game extends ConfiggyGroup {
	  var deathpenalty = 2
	  var startcost = 10
	  var refund = 50
	  var accountlimit = 1000
	  var recruitshare = 50
	  override def update(conf : Reconfigure) {
		   deathpenalty = conf get "deathpenalty" or deathpenalty
		   startcost = conf get "startcost" or startcost
		   refund = conf get "refund" or refund
		    accountlimit = conf get "accountlimit" or accountlimit
		    recruitshare = conf get "recruitshare" or recruitshare
	  }
	}
	object market extends ConfiggyGroup {
	  var planesfile = "planes.lst"
	  var unlimitedquanity = 50
	  var maxStartPrice = 50
	  var maxPrice = 100
	  var updatePeriod = 5
	  var maxStep = 5
	  var tolerance = 20 
   
	  override def update(conf : Reconfigure) {
	  	planesfile = conf get "planesfile" or planesfile
	  	unlimitedquanity = conf get "unlimitedquanity" or unlimitedquanity
	  	maxStartPrice = conf get "maxStartPrice" or maxStartPrice
	  	maxPrice = conf get "maxPrice" or maxPrice
	  	updatePeriod = conf get "updatePeriod" or updatePeriod
	  	maxStep = conf get "maxStep" or maxStep
	  	tolerance = conf get "tolerance" or tolerance
	  }
	} 
	
	def apply(conf : configgy.Config) = {
	  val list = List(server, game, market)
	  list foreach (_ apply conf)
	}
	try{
		this(configgy.Config.fromFile(file))
	}catch{
	  case x: _root_.java.io.IOException => x.printStackTrace
	}
	var multi = new multiplexer.Multiplexer(server.host, server.il2port, server.consoleport)
 
}

