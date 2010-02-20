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

		object state{
			var deathPause = false
			var deathPauseUntil = 0L
			
			var died = false
			var landed = false
			
			var planeName = ""
			var planeSince = System.currentTimeMillis
			var planeAllowed = true
			var oldPlane = "alwaysNew"
			var planePrice : Double = 0
			
			def dies = {
			  deathPauseUntil = server.rules.calculateDeathPause
			  val seconds = (deathPauseUntil - System.currentTimeMillis) / 1000 
			  val priceMsg = planeLost
			  
			  priceMsg match{
			    case Some(msg) => {
			      chat(name+": death pause for "+seconds+"s and "+msg)
			    }
			    case None => {
			      chat(name+": death pause for "+seconds+"s, then clear to fly "+oldPlane)
			    } 
			  }
			}
			private def planeLost : Option[String] = {
			  if(planeAllowed && ! deathPause && somePlane) applyTime// finish balance
				planeAllowed = true
				refund () = 0    
				
				val result =server.rules.startCostCheck(planePrice, balance) match {
				  case Rules.CostResult(false, _, _, cost) => {
				    Some("you can't afford a "+planeName+" again")//+", chat \"! available\" for information")
				  }
				  case _ => None
				}
    
				oldPlane = planeName
				planeName = ""
				result
			}
			def crashes = {
				val priceMsg = planeLost
    
				priceMsg.map{msg => 
				  chat(name+": "+msg+", chat \"! available\"")
				}
    	}
			
			var since = System.currentTimeMillis
			def noPlane =  planeName==null || planeName.trim.isEmpty 
			def somePlane =  ! noPlane
			
			
			def applyTime(){
				val now = System.currentTimeMillis
				
				val millis = System.currentTimeMillis - planeSince
				val difference = planePrice * millis / (-60000) 
				debug("price update for "+millis+" with raw price"+ planePrice+ " -> difference "+difference )       
				balance () = server.rules.updateBalance(balance, difference)
				planeSince = now
			}
			// call when it is definitely known that the pilot is fresh in a plane
			def enteringPlane {
				refund () =0
				invitations.retain(((x,y) => y.until > System.currentTimeMillis))
				planePrice = server.market.getPrice(planeName)
				oldPlane = ""
				val now = System.currentTimeMillis
				planeSince = now
				
				if(deathPauseUntil>now){
					// pilot is in death pause, invitations are checked in rules
					deathPause = true
					planeAllowed = false
				} else {
					deathPause = false
					server.rules.startCostCheck(planePrice, balance) match {
					case Rules.CostResult(true, newBalance, newRefund, startFee) => {
							planeAllowed = true
							if(newBalance!=balance.value){
								if(newRefund>0) chat("Start fee "+startFee+conf.names.currency+", possible refund: "+newRefund +conf.names.currency+"")
								else chat("Start fee of "+startFee+conf.names.currency+" debited")
							}
							balance () = newBalance
							refund () = newRefund
						}
					case Rules.CostResult(false, newBalance, newRefund, startFee) => {
							chat(""+startFee+conf.names.currency+" needed, available "+(startFee-balance.value)+conf.names.currency+"")
							
							planeAllowed = false
						}
					}
				}          
			}
			def planePossiblyOld = planeName != oldPlane
			def updatePlaneName(what:String){
				if(what==null || what.trim.isEmpty){
					planeAllowed = true
					planeName = ""
					oldPlane = ""
					refund () =0
				}else what match {   
					case existingPlane if(existingPlane==planeName) => { // plane name did not change
						if( existingPlane!=oldPlane ) enteringPlane
					}
					case newPlane => {
						planeName = newPlane
						enteringPlane
					}
				}
				if(deathPause ) server.rules.warnDeath(Pilot.this.name, planeName, since, deathPauseUntil, invitations.value)
				else if( ! planeAllowed ) server.rules.warnPlane(Pilot.this.name, planeName, since, balance)
			}

			def returns(){
				if(refund.value>0) {
					chat("Awarded a refund of "+refund.value+conf.names.currency+" for returning the "+planeName)
					balance () = server.rules.updateBalance(balance, refund)
					refund () = 0
				}
				oldPlane = ""
			} 
		}
  
		def priceMessages(which:String, all:Boolean){
			val pilotName = Pilot.this.name
   
			server.planes.forMatches(which){plane =>		  
				//debug(name+" pricecommand: '"+which+"'")   		    	    
				val price = server.market.getPrice(plane.name)

				def padRight(in:String, reference:String):String=in+(reference.drop(in.length))
				def padLeft(in:String, reference:String):String=(reference.drop(in.length))+in
				val (result, affordable, verb) = (if(price > 0){
					server.rules.startCostCheck(price, balance) match {
					case Rules.CostResult(false, newBal, _, startFee) => {
							(false, "!", "would.cost."+startFee.toInt+".+" )
						}
					case Rules.CostResult(true, _, _, startFee) => {
							(true, "+", "costs.once."+startFee.toInt+".+" )
						}
					case _ => (false, "!","gives u respect, yo!")
					}
				}else{
					(true, "*", "gives")
				})
				
    
				if(all||result){
				  // padding for longest possible name:
					//                                   P_40SUKAISVOLOCHHAWKA2
					val paddedPlane = padRight(plane.name, "......................")
					//                              would cost 1000 +  
					val paddedVerb = padLeft(verb, ".................")
					
					var intPrice = price.toInt
					intPrice = intPrice.abs
					val paddedPrice = padLeft(""+price.abs.toInt,".....")
					val msg = affordable+" "+paddedPlane+paddedVerb+paddedPrice+conf.names.currency+".per.minute"  
					//server.multi ! server.multi.ChatTo(pilotName, msg)
					chat(msg)	 
				}
			}
		}
  
		override def messageHandler = new PartialFunction[Any, Unit]{
			override def isDefinedAt(x:Any) = {
debug("pilot "+name +" <- "+x)		    
				ImessageHandler isDefinedAt x
				
			}
			override def apply(x:Any) = ImessageHandler.apply(x)
		}

		def ImessageHandler : PartialFunction[Any, Unit] = { 
			case Is.Persisted =>  

			case Is.Flying(plane, army) => {
				currentSide_=( army )
				state.updatePlaneName(plane)
			}
			case Is.Returning => {
				state.returns
			}
			case Is.Dying => {
				state.dies
			}
			case Is.Crashing => {
				state.crashes
			}
			case inv : Pilots.Invitation => {
				invitations.put(inv.plane, inv)
			  if(state.deathPause) chat(inv.by + " invites you to fly "+inv.plane+" ("+((inv.until - System.currentTimeMillis)/1000)+"s)")
			}


			case Is.Chatting(msg) => msg match { 
				case Pilots.Commands.balancecommand(_) => {
					chat("current balance is "+balance.value)
				}
				case Pilots.Commands.pricecommand(which) => {
					priceMessages(which, true)
				}
				case Pilots.Commands.availablecommand(which) => {
					priceMessages(which, false)
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
		val availablecommand = """\s*!\s*available\s+(\S*)""".r
	}
	class Invitation(val by:String, val plane:String, val until:Long) 
}

