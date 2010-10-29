package de.immaterialien.sturmonanny.core 

import _root_.de.immaterialien.sturmonanny.util._
import _root_.de.immaterialien.sturmonanny.core._ 
import _root_.de.immaterialien.sturmonanny.fbdjhosting.FbdjAdapter 
import _root_.de.immaterialien.sturmonanny.persistence.BalanceWrapper 
  
class Server(val initConf : String, val threadGroup:java.lang.ThreadGroup) extends Logging{
  def this(initConf:String) = this(initConf, null)
	def this() = this("default.conf") 
	private var members : List[UpdatingMember] = Nil
	this : Server
 
	private var internalconf = new Configuration(initConf, this)  
	def conf = internalconf  
	def conf_= (newConf : Configuration) {  
		internalconf = newConf
		members foreach (_ updateConfiguration)
	}   
	def shutdown{
	  if(threadGroup!=null) threadGroup.interrupt
   else error("can only shut down if started within a threadgroup")
	} 
	val balance = new BalanceWrapper with Member 
	val rules = new Rules with Member   
	val pilots = new Pilots with Member   
	val planes = new Planes with Member      
	val market = new MarketActor(conf.market.implementation.apply, conf.market.configuration.apply) with Member
	val fbdj = new FbdjAdapter with Member
	val dispatcher = new LocalizedDispatcher with Member
	val eventlog = new EventLogDispatcher with Member


	val multi = new Multiplexer ("", 0, conf.server.consoleport.apply) with Member
 
//	debug("conf is initialized from '"+initConf+"'\n================\n"+conf)
	members = members.reverse // so that member initialization happens in the order in which they are declared
	for(m <- members){
//		debug("initializing configuratoin for "+m.getClass.getSuperclass.getSimpleName+" <- "+m.getClass.getInterfaces.map(x=>{x.getSimpleName}).foldLeft("")((a, b)=>a+"-" + b))      
		m.updateConfiguration
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
        	result = new Server(conf, tg)
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



