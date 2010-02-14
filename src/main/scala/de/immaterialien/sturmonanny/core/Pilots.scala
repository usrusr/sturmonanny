package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

 

 
          
class Pilots extends Domain[Pilots] with NonUpdatingMember with Logging{
	override def newElement(name:String) = new Pilot(name)
	class Pilot(override val name : String) extends Pilots.this.Element(name) with SideProvider{
		 
    
//        val planes = Army Val new mutable.LinkedHashMap[String, PlaneState]
        val died = Army Var 0   
		val balance = Army Var 0D
		val refund = Army Var 0D
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
//		   var refund : Double = 0.
     
		     def applyTime(){
			    val now = System.currentTimeMillis
			    val price = server.market.getPrice(plane.name)
			    val millis = System.currentTimeMillis - plane.since
			    val difference = price * millis / (-60000) 
debug("price update for "+millis+" with raw price"+ price+ " -> difference "+difference )       
			    balance () = server.rules.updateBalance(balance, difference)
			    plane.since = now
			}
     
     
		   def flies(what:String){
		     what match {
		       case "" => {
		         allowed = true
		         name = ""
		         refund () =0
		       }
		       case newPlane if(name!=newPlane)=> {
		    	 since = System.currentTimeMillis 
    	         val price : Double= server.market.getPrice(newPlane)
		         server.rules.startCostCheck(price, balance) match {
		           case Rules.CostResult(allowedresult, newBalance, newRefund, startFee) => {
		             if(allowedresult){
			             allowed = true
			             if(newBalance!=balance.value){
			               if(newRefund>0) chat("Start fee "+startFee+"%s, possible refund: "+newRefund +"%s")
			               else chat("Start fee of "+startFee+"%s debited")
			             }
			             balance () = newBalance
			             refund () = newRefund
			         }else{
		        	 chat(""+startFee+"%s needed, available "+(startFee-balance.value)+"%s")
		           
		             allowed = false
		             refund () =0
			         }
		           }
		         }
		    	 name = newPlane
		       }
		       case name => {
			     if(allowed) applyTime
		       }
		     }
		     if( ! allowed ){
		       server.rules.warnPlane(Pilot.this.name, plane.name, since, balance)
		     }
		   }
		   def returns(){
		     if(refund.value>0) {
		       chat("Awarded a refund of "+refund.value+"%s for returning the "+plane.name)
		       balance () = server.rules.updateBalance(balance, refund)
		       refund () = 0
		     }
		   } 
		   def dies{
		     flies(name)// finish balance
		     name = ""
		     allowed = true
		     refund () = 0
		   }
		   def warn{
		     
		   }
		}
    
		override def messageHandler = { 
		  case Is.Persisted =>  
 
   		  case Is.Flying(plane, army) => {
   	 	    currentSide_=( army )
   	 	    this.plane flies plane
          }
   		  case Is.Returning => {
   		    plane.returns
   		  }
          case Is.Destroyed => {
   			  plane.dies
          }

//   		  case server.warning.passed  => if(System.currentTimeMillis<deathPauseUntil){
//   		    
//   		  }   
   		  case Is.Chatting(msg) =>
   		    msg match { 
   		    	case Pilots.Commands.balancecommand(_) => {
   		    	   
   		    	  chat("current balance is "+balance.value)
   		    	}
   		    	case Pilots.Commands.pricecommand(which) => {
   		    		val pilotName = Pilot.this.name
   		    	  server.planes.forMatches(which){plane =>
//debug(name+" pricecommand: '"+which+"'")   		    	    
   		    	    val price = server.market.getPrice(plane.name)
              		def padRight(in:String, reference:String):String=in+(reference.drop(in.length))
              		def padLeft(in:String, reference:String):String=(reference.drop(in.length))+in
   		    	    val (affordable, verb) = if(price > 0){
   		    	    	server.rules.startCostCheck(price, balance) match {
				           case Rules.CostResult(false, newBal, _, startFee) => {
				             ("!", "would.cost."+startFee.toInt+".+" )
				           }
				           case Rules.CostResult(true, _, _, startFee) => {
				             ("+", "costs.once."+startFee.toInt+".+" )
				           }
				           case _ => ("!","gives u respect, yo!")
				         }
   		    	    }else{
   		    	      ("*", "gives")
   		    	    }
              		
              	    // padding for longest possible name:
                    //                                   P_40SUKAISVOLOCHHAWKA2
                    val paddedPlane = padRight(plane.name, "......................")
                    //                              would cost 1000 +  
                    val paddedVerb = padLeft(verb, ".................")
                    
                    var intPrice = price.toInt
                    intPrice = intPrice.abs
                    val paddedPrice = padLeft(""+price.abs.toInt,".....")
   		    	    val msg = affordable+" "+paddedPlane+paddedVerb+paddedPrice+"%s.per.minute"  
            		//server.multi ! server.multi.ChatTo(pilotName, msg)
            		chat(msg)
   		    	  }
   		    	}
   		    	case x => //debug("unknown command by "+name+":"+x)
   		    } 
   		  
		  case _ => unknownMessage _ 
		}
  
		def deathPauseFunction : PartialFunction[Any, Unit] = {
		  
//		  { 
//		    case Multiplexer.minute.passed(_) => 
//		  }
        case _ =>
		}
//		class PlaneState(private var total : Long){
//		  private var flying = false
//		  private var since = 0L
//		  private var lastUpdate = 0L
//		  def flies {
//		    if(flying){ 
//		      val cur = System.currentTimeMillis
//		      total += cur - lastUpdate
//		      lastUpdate = cur
//		    }else{
//		      since = System.currentTimeMillis
//		      lastUpdate = 0L
////		      server.warning.subscribe(Pilot.this)
//		    }
//		  }
//		} 
		def checkDeathPause {
		  
		}
		private def chat(msg:String) = server.multi ! server.multi.ChatTo(name, msg) 
		
	}
}
object Pilots {
  object Commands{
	  val balancecommand = """(\s*!\s*balance\s*)""".r
	  val pricecommand = """\s*!\s*price\s+(\S*)""".r
  }

}

