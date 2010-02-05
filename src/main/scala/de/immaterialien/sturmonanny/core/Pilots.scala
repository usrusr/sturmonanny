package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

 
object DIES 
case class chats(val msg : String) 
case class join(val side : Armies.Armies)   
case class inform(val text : String, val to : String)

         
class Pilots extends Domain[Pilots] with NonUpdatingMember with Logging{
	override def newElement(name:String) = new Pilot(name)
	class Pilot(override val name : String) extends Pilots.this.Element(name) with SideProvider{
		 
   
        val planes = Army Val new mutable.LinkedHashMap[String, PlaneState]
        val currentPlane = Army Var ""
        val died = Army Var 0   
		val balance = Army Var 0D
		var deathPauseUntil = 0L
   
		override def defaultMessageHandler = { 
		  case PERSIST =>  

   		  case DIES => {  
   		    died() = died+1  
   		    deathPauseUntil = System.currentTimeMillis + (conf.game.deathpenalty * 1000)
   		    currentPlane () = ""
          }
   		  case flies(plane, army) => {
   	 	    currentSide_=(army)
   	 	    currentPlane other = ""
   	 	    currentPlane () = plane

   		    planes.value.getOrElseUpdate(plane, new PlaneState(0)).flies 
          }
   		  case server.warning.passed  => if(System.currentTimeMillis<deathPauseUntil){
   		    
   		  }   
   		  case chats(msg) =>
   		    msg match {
   		    	case Pilots.Commands.balancecommand(_) => {
   		    	  
   		    	  val reply = server.multi.ChatTo(name, "current balance is "+balance.value)
debug(name+" balance "+reply)   		    
   		    	  server.multi !  server.multi.ChatTo(name, "current balance is "+balance.value)
   		    	}
case x => debug("unknown command  "+x)
   		    } 
   		  
		  case _ => unknownMessage _ 
		}
  
		def deathPauseFunction : PartialFunction[Any, Unit] = {
		  
//		  { 
//		    case Multiplexer.minute.passed(_) => 
//		  }
        case _ =>
		}
		class PlaneState(private var total : Long){
		  private var flying = false
		  private var since = 0L
		  private var lastUpdate = 0L
		  def flies {
		    if(flying){ 
		      val cur = System.currentTimeMillis
		      total += cur - lastUpdate
		      lastUpdate = cur
		    }else{
		      since = System.currentTimeMillis
		      lastUpdate = 0L
		      server.warning.subscribe(Pilot.this)
		    }
		  }
		} 
	}
}
object Pilots {
  object Commands{
	  val balancecommand = """(\s*!\s*balance\s*)""".r
	  val pricecommand = """\s*!\s*price\s+(\S+)""".r
  }
}

