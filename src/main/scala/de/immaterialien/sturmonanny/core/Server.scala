package de.immaterialien.sturmonanny.core 

import de.immaterialien.sturmonanny.util._
import de.immaterialien.sturmonanny.core._ 

class Server(val initConf : String) {  
	def this() = this("default.conf")
	private var members : List[UpdatingMember] = Nil
	this : Server
	private var internalconf = new Configuration(initConf)
    def conf = internalconf 
    def conf_= (newConf : Configuration) {
      internalconf = newConf
      members foreach (_ updateConfiguration)
    } 
 
 
	val multi = new Multiplexer (conf.server.host, conf.server.il2port, conf.server.consoleport) with Member 
 
    val market = new MarketActor with Member       
    val pilots = new Pilots with Member 
    val planes = new Planes with Member     
    val dispatcher = new Dispatcher with Member
    
    val warning = new ConfigurableTimerActor(conf.game.warningInterval) with Member
    val minute = new ConfigurableTimerActor(60000) with Member

//    members foreach (_ updateConfiguration)
    /**
	 * mix in Member to connect the UpdatingMember to this   
	 */
    trait Member extends UpdatingMember{
		override val server = Server.this
		override def conf  = server.conf
		server.members ::= this 
    }
}
trait NonUpdatingMember extends UpdatingMember {
  override def updateConfiguration=()
} 
trait UpdatingMember { 
  def updateConfiguration : Unit 
  val server : Server = null // overridden by Server#Member
  def conf  : Configuration = Configuration.Default
}

class ConfigurableTimerActor(extractor : => Int) extends TimerActor(extractor) with UpdatingMember {
  def updateConfiguration = setInterval(extractor)
}
