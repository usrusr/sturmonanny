package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

 
case class chats(val msg : String) 
case object lands
case object dies
case object ejects
case object crashes
case class join(val side : Armies.Armies)   
case class inform(val text : String, val to : String)
 
          
class Pilots extends Domain[Pilots] with NonUpdatingMember with Logging{
	override def newElement(name:String) = new Pilot(name)
	class Pilot(override val name : String) extends Pilots.this.Element(name) with SideProvider{
		 
    
//        val planes = Army Val new mutable.LinkedHashMap[String, PlaneState]
        val died = Army Var 0   
		val balance = Army Var 0D
		var deathPauseUntil = 0L
//		var loggedInPlane : Option[String] = None 
//		/**
//		 * a plane the pilot is not allowed to fly 
//		 */
//		var forbiddenPlane : Option[String] = None 
  
		object plane{
		   var allowed = true
		   var name = ""
		   var since = System.currentTimeMillis
		   var refund : Double = 0.
     
		     def updateBalance(){
			    val price = server.market.getPrice(plane.name)
			    val difference = System.currentTimeMillis - plane.since
			    
			    balance () = server.rules.updateBalance(balance, difference)
			}
     
     
		   def flies(what:String){
		     what match {
		       case "" => {
		         allowed = true
		         name = ""
		       }
		       case newPlane if(name!=newPlane)=> {
		    	 since = System.currentTimeMillis 
    	         name = newPlane
		         server.rules.startCostCheck(what, balance) match {
		           case Rules.CostResult(false, _, _) => {
		             allowed = false
		             refund = 0.
		           }
		           case Rules.CostResult(true, newBalance, newRefund) => {
		             allowed = true
		             balance () = newBalance
		             refund = newRefund
		           }
		         }
		       }
		       case name => {
			     if(allowed) updateBalance
		       }
		     }
		     if( ! allowed ){
		       server.rules.warnPlane(Pilot.this.name, plane.name, since, balance)
		     }
		   }
		   def dies{
		     flies(name)
		     name = ""
		     allowed = true
		   }
		   def warn{
		     
		   }
		}
    
		override def messageHandler = { 
		  case PERSIST =>  
 
   		  case this.died => {
   			  plane.dies
          }
   		  case flies(plane, army) => {
   	 	    currentSide_=( army )
   	 	    this.plane flies plane
          }
//   		  case server.warning.passed  => if(System.currentTimeMillis<deathPauseUntil){
//   		    
//   		  }   
   		  case chats(msg) =>
   		    msg match { 
   		    	case Pilots.Commands.balancecommand(_) => {
   		    	   
   		    	  val reply = server.multi.ChatTo(name, "current balance is "+balance.value)
debug(name+" balance "+reply+ " in market +" + server.market.internal)   		    
   		    	  server.multi !  server.multi.ChatTo(name, "current balance is "+balance.value)
   		    	}
   		    	case Pilots.Commands.pricecommand(which) => {
   		    	  val list = new scala.collection.jcl.TreeSet[String]()
   		    	  server.planes.forMatches(which){plane =>
   		    	    val price = server.market.getPrice(plane.name)
   		    	    val affordable = if(price > 0){
   		    	    	server.rules.startCostCheck(plane.name, balance) match {
				           case Rules.CostResult(false, _, _) => {
				             ":-("
				           }
				           case Rules.CostResult(true, newBalance, newRefund) => {
				             ";^)"
				           }
				           case _ => ""
				         }
   		    	    }else{
   		    	      ":-)"
   		    	    }
   		    	    val msg = plane.name +" costs "+price+"%s per minute "+affordable  
   		    	    list.add(msg)
   		    	  }
   		    	  for(line <- list){
   		    	    server.multi ! server.multi.ChatTo(Pilot.this.name, line)
debug(name+" balance for "+which+": "+line)               
   		    	  }
   		    	}
case x => debug("unknown command by "+name+":"+x)
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
//		      server.warning.subscribe(Pilot.this)
		    }
		  }
		} 
		def checkDeathPause {
		  
		}
		
	}
}
object Pilots {
  object Commands{
	  val balancecommand = """(\s*!\s*balance\s*)""".r
	  val pricecommand = """\s*!\s*price\s+(\S*)""".r
//	  val pricescommand = """\s*!\s*prices\s+(\S+)""".r  
  }

}

