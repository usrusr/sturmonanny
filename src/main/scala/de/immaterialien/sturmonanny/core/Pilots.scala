package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

 

 
          
class Pilots extends Domain[Pilots] with NonUpdatingMember with Logging{
	override def newElement(name:String) = new Pilot(name)
	class Pilot(override val name : String) extends Pilots.this.Element(name) with SideProvider{
		val balance = Army Var 0D
		val refund = Army Var 0D
		val invitations = Army Val (new mutable.HashMap[String, Pilots.Invitation]())
  
    	object pilot{
     	  var deathPause = false
     	  var deathPauseUntil = 0L
  
    	  def dies = {
    	    deathPauseUntil = server.rules.calculateDeathPause
    	    plane.dies // if the pilot dies the plane is lost as well (forget the corner case of a headshot @ base!)
    	  }
    	}
		object plane{
			var allowed = true
		   	var name = ""
		   	var since = System.currentTimeMillis
//		   var refund : Double = 0.
			def none =  name==null || name.trim.isEmpty 
			def some =  ! none
			
     
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
		     val now = System.currentTimeMillis
		     invitations.retain(((x,y) => y.until > now))
		     
		     if(what==null || what.trim.isEmpty){
		         allowed = true
		         name = ""
		         refund () =0
		     }else what match {
		       case newPlane if(name!=newPlane)=> {
		    	 since = now
		    	 name = newPlane
		         if(pilot.deathPauseUntil>now){
			    	 pilot.deathPause = true
			    	 allowed = false
			     } else {
			         pilot.deathPause = false
		         
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
		         }
		       }
		       case name => {
			     if(allowed) applyTime
		       }
		     }
		     if(pilot.deathPause ) server.rules.warnDeath(Pilot.this.name, plane.name, since, pilot.deathPauseUntil, invitations.value)
		     else if( ! allowed ) server.rules.warnPlane(Pilot.this.name, plane.name, since, balance)
		   }
		   def returns(){
		     if(refund.value>0) {
		       chat("Awarded a refund of "+refund.value+"%s for returning the "+plane.name)
		       balance () = server.rules.updateBalance(balance, refund)
		       refund () = 0
		     }
		   } 
		   def dies{
		     if(allowed && ! pilot.deathPause && plane.some) flies(name)// finish balance
		     name = ""
		     allowed = true
		     refund () = 0
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
          case Is.Dying => {
   			  plane.dies
   			  pilot.dies
          }
          case Is.Crashing => {
        	  plane.dies
          }
          case inv : Pilots.Invitation => {
            invitations.put(inv.plane, inv)
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
  

		private def chat(msg:String) = server.multi ! server.multi.ChatTo(name, msg) 
		
	}
}
object Pilots {
  object Commands{
	  val balancecommand = """(\s*!\s*balance\s*)""".r
	  val pricecommand = """\s*!\s*price\s+(\S*)""".r
  }
  class Invitation(val by:String, val plane:String, val until:Long) 
}

