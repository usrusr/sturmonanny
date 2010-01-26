package de.immaterialien.sturmonanny.multiplexer


import de.immaterialien.sturmonanny.util.ConfiggyGroup

class Server {
	var members : List[UpdatingMember] = Nil
	this : Server
	private var internalconf = new Configuration("default.conf")
	val multi = new multiplexer.Multiplexer (conf.server.host, conf.server.il2port, conf.server.consoleport) with Member
 
    val market = new MarketActor with Member 
    val pilots = new Pilots with Member
    val planes = new Planes with Member
 
    val warning = new util.TimerActor(conf.game.warningInterval)  with Member {
    	override def updateConfiguration = setInterval(conf.game.warningInterval)
    }
    val minute = new util.TimerActor(60000) with Member with NonUpdatingMember

     
     
    def conf = internalconf 
    def conf_= (newConf : Configuration) {
      internalconf = newConf
      members foreach (_ updateConfiguration)
    } 

    /**
	 * mix in Member to connect the UpdatingMember to this   
	 */
    trait Member extends UpdatingMember{
		override val server : Server = Server.this
		override def conf  = server.conf
		server.members ::= this 
    }
}
trait NonUpdatingMember extends UpdatingMember {
  override def updateConfiguration : Unit = { }
}
trait UpdatingMember {
  def updateConfiguration : Unit 
  val server : Server = null // overridden by Server#Member
  def conf  : Configuration = Configuration.Default
}

