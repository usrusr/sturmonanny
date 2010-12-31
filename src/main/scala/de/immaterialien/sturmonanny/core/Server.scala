package de.immaterialien.sturmonanny.core 

import _root_.de.immaterialien.sturmonanny.util._
import _root_.de.immaterialien.sturmonanny.core._ 
import _root_.de.immaterialien.sturmonanny.fbdjhosting.FbdjAdapter 
import _root_.de.immaterialien.sturmonanny.persistence.BalanceWrapper 
  


trait Server extends Logging{ 
	def conf : Configuration 
	def conf_= (newConf : Configuration)
	def shutdown
	
	private var members : List[UpdatingMember] = Nil
	
	val balance : BalanceWrapper with Member 
	val rules : Rules with Member   
	val pilots : Pilots with Member   
	val planes : Planes with Member      
	val market : MarketActor with Member
	val fbdj : FbdjAdapter with Member
	val dispatcher : LocalizedDispatcher with Member
	val eventlog : EventLogDispatcher with Member
	val multi : Multiplexer with Member
	val time : TimeSource //with Member
 
debug("init server trait")	

	protected def init(){
		//	debug("conf is initialized from '"+initConf+"'\n================\n"+conf)
		members = members.reverse // so that member initialization happens in the order in which they are declared
		for(m <- members){
			debug("initializing configuratoin for "+m.getClass.getSuperclass.getSimpleName+" <- "+m.getClass.getInterfaces.map(x=>{x.getSimpleName}).foldLeft("")((a, b)=>a+"-" + b))      
			m.updateConfiguration
		} 		
	} 
	/**
	* mix in Member to connect the UpdatingMember to this   
	*/
	trait Member extends UpdatingMember{
		override val server = Server.this
		override def conf  = server.conf
		server.members ::= this 
	}	
}
class ServerImpl(val initConf : String, val threadGroup:java.lang.ThreadGroup) extends Server{ Server =>
  def this(initConf:String) = this(initConf, null)
	def this() = this("default.conf") 
	private var members : List[UpdatingMember] = Nil 
	//this : Server =>
 
	private var internalconf = new Configuration(initConf, this)  
	override def conf = internalconf  
	override def conf_= (newConf : Configuration) {  
		internalconf = newConf
		members foreach (_ updateConfiguration)
	}   
	override def shutdown{
	  if(threadGroup!=null) threadGroup.interrupt
   else error("can only shut down if started within a threadgroup")
	} 
	override val balance = new BalanceWrapper with Member 
	override val rules = new Rules with Member   
	override val pilots = new Pilots with Member   
	override val planes = new Planes with Member      
	override val market = new MarketActor(conf.market.implementation.apply, conf.market.configuration.apply) with Member
	override val fbdj = new FbdjAdapter with Member
	override val dispatcher = new LocalizedDispatcher with Member
	override val eventlog = new EventLogDispatcher with Member
	override val time = new TimeSourceImpl

	override val multi = new Multiplexer ("", 0, 0) with Member
debug("init server class")	 
	init()
} 
trait NonUpdatingMember extends UpdatingMember {
	override def updateConfiguration=() 
} 
trait UpdatingMember {  
	def updateConfiguration : Unit 
	val server : Server = null // overridden by Server#Member
	def conf  : Configuration = Configuration.default(server)
}

object Server {
  def withThreadGroup(conf:String):Server={
    val notifier = new java.lang.Object
    var result :Server=null
    var throwable:Throwable=null
    val tg = new ThreadGroup("server "+conf)
    val t = new Thread(tg, "server "+conf+" init"){
      override def run={
        try{
        	result = new ServerImpl(conf, tg)
        }catch{
          case x => {
            throwable = x
          }
        }finally {
//println("done "+throwable+" -> "+result)      
	        notifier.synchronized{
	          notifier.notifyAll
	        }
        }
      }
    }
    t.start
    notifier.synchronized{
      notifier.wait
//println("started in thread group "+throwable+" -> "+throwable.getCause)      
      if(throwable!=null) throw throwable
      if(result==null) throw new net.lag.configgy.ParseException("server isntance for "+conf+" could not be created")
      result
    }
  }
}



