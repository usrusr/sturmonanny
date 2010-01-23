package de.immaterialien.sturmonanny.multiplexer

import de.immaterialien.sturmonanny.model.Configuration
import de.immaterialien.sturmonanny.util.ConfiggyGroup

class Server {
	var members : List[Member] = Nil
	this : Server
	private var internalconf = new Configuration("default.conf")
	val multi = new multiplexer.Multiplexer (conf.server.host, conf.server.il2port, conf.server.consoleport) with this.MyMember
    val warning = new util.TimerActor(conf.game.warningInterval)  with this.MyMember {
    	override def updateConfiguration = setInterval(conf.game.warningInterval)
    	
    }

    
     
    def conf = internalconf
    def conf_= (newConf : Configuration) {
      internalconf = newConf
      members foreach (_ updateConfiguration)
    } 
    
    trait MyMember extends Member{
		override def server : Server = Server.this
		override def conf  = server.conf
		server.members ::= this 
		
		
    }
}
trait Member {
  def server : Server = null
  def conf  : Configuration = Configuration.Default
  def updateConfiguration : Unit 
}
