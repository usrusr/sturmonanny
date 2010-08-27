package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

import net.liftweb.actor



class Pilots extends Domain[Pilots] with actor.LiftActor with NonUpdatingMember with Logging{
	case class ClearToFly(pilot:Pilot, deathPauseUntil:Long)
	override def messageHandler = {
		case ClearToFly(pilot, deathPauseUntil) => if(deathPauseUntil == pilot.state.deathPauseUntil){
			server.multi ! new server.multi.ChatTo(pilot.name, "clear for takeoff")
		}
	}
	override def newElement(name:String) = new Pilot(name)
	class Pilot(override val name : String) extends Pilots.this.Element(name) with SideProvider{
		val balance = Army Var 0D
		val refund = Army Var 0D
		val invitations = Army Val (new mutable.HashMap[IMarket.Loadout, Pilots.Invitation]())
		
		for(loaded <- server.balance.load(name)){
			balance(Armies.RedSide) = loaded.red
			balance(Armies.BlueSide) = loaded.blue
			val currency = conf.names.currency
			chat("your balance: "+loaded.red.toInt+currency+" on red and "+loaded.blue.toInt+currency+" on blue")
		}
		

		object state{
		  override def toString = {
		    " "+
		    (if(deathPause) ("pause for "+(deathPauseUntil-System.currentTimeMillis)) else "") +
        (if(died) " died" else "") +
        (if(landed) " landed" else "") +
        " plane:" +planeName+" @ "+planePrice + 
        (if(planeVerified) " verified" else " unchecked") +
        (if(lostPlaneName!="") " lost:"+lostPlaneName else "") +
        ""
		  }
		  
			var deathPause = false
			var deathPauseUntil = 0L
			
			var died = false
			var landed = false
			var flying = false
			
			var planeName = ""
			var lastPlanePriceCommit = System.currentTimeMillis
			var planeVerified = true
			var lostPlaneName = ""
			var planePrice : Double = 0
			var load : Option[String] = None
			var lastBalance : Option[Double] = None
			
			def dies() = {
				val now = System.currentTimeMillis
				if(deathPauseUntil<=now){
				  deathPauseUntil = server.rules.calculateDeathPause
				  
				  actor.LAPinger.schedule(Pilots.this, ClearToFly(Pilot.this, deathPauseUntil), deathPauseUntil-now)
				  
				  val seconds = (deathPauseUntil - now) / 1000 
				  val priceMsg = planeLost()
	
				  priceMsg match{
				    case Some(msg) => {
				      chat(name+": death pause for "+seconds+"s and "+msg)
				    }
				    case None => {
				      chat(name+": death pause for "+seconds+"s, then clear to refly "+lostPlaneName)
				    } 
				  }
				}
				flying=false
			}
			private def planeLost() : Option[String] = {
				flying=false
			  if(planeVerified && ! deathPause && somePlane) commitPlanePrice()// finish balance

				
				val result =server.rules.startCostCheck(planePrice, balance) match {
				  case Rules.CostResult(false, _, _, cost) => {
				    Some("you can't afford a "+planeName+" again")//+", chat \"! available\" for information")
				  }
				  case _ => None
				}
			  refund () = 0    
				lostPlaneName = planeName
				planeVerified = false
				planeName = ""
				planePrice = 0
				load = None
				result
			}
			def crashes() = {
				val priceMsg = planeLost()
    
				priceMsg.map{msg => 
				  chat(name+": "+msg+", chat \"! available\"")
				}
				flying=false
    	}
			

			def noPlane =  planeName==null || planeName.trim.isEmpty 
			def somePlane =  ! noPlane
			
			
			def commitPlanePrice(){
			  if(planeVerified){
println("commit plane price "+state)			  	
					val now = System.currentTimeMillis

					val millis = System.currentTimeMillis - lastPlanePriceCommit
					val difference = planePrice * millis / (-60000) 
					debug("price update for "+millis+" with raw price"+ planePrice+ " -> difference "+difference )       
					balance () = server.rules.updateBalance(balance, difference)
					lastPlanePriceCommit = now
					persist()
			  }
			}
			// call when it is definitely known that the pilot is fresh in a plane
			def definitelyInPlane() {
				refund () =0
				invitations.retain(((x,y) => y.until > System.currentTimeMillis))
				planePrice = server.market.getPrice(planeName, load)
				lostPlaneName = ""
				val now = System.currentTimeMillis
				lastPlanePriceCommit = now
				
				if(deathPauseUntil>now){
					// pilot is in death pause, invitations are checked in rules
					deathPause = true
					planeVerified = false
				} else {
					deathPause = false
					server.rules.startCostCheck(planePrice, balance) match {
					case Rules.CostResult(true, newBalance, newRefund, startFee) => {
							planeVerified = true
							if(newBalance!=balance.value){
								if(newRefund>0) chat("Start fee "+startFee+conf.names.currency+", possible refund: "+newRefund +conf.names.currency+"")
								else chat("Start fee of "+startFee+conf.names.currency+" debited")
							}
							lastBalance = Some(balance)
							balance () = newBalance
							refund () = newRefund
						}
					case Rules.CostResult(false, newBalance, newRefund, startFee) => {
							chat(""+startFee+conf.names.currency+" needed, available "+(balance.value)+conf.names.currency+"")
							
							planeVerified = false
						}
					}
				}          
			}
			def planeNotEmpty = ! (planeName==null || planeName=="")
			def planeNotLostPlane = lostPlaneName==null || lostPlaneName=="" || planeName != lostPlaneName
			
			def updateLoadout(what:String){
				
				load = Some(what)
				if(planeVerified){
					val newPrice = server.market.getPrice(planeName, load)
					if(planePrice != newPrice ){
						if(lastBalance.isDefined){
							balance () = lastBalance.get
							refund () = 0
							chat("balance rollback due to updated loadout information")
							definitelyInPlane()
						}else{
							debug("lastBalance is undefined while plane is verified")
						}
					}
				}
			}
			def updatePlaneName(what:String){
				if(what==null || what.trim.isEmpty){
					planeVerified = false
					planeName = ""
					lostPlaneName = ""
					refund () =0
				}else if(what==planeName) { // plane name did not change
//						if(( ! planeVerified) && planeNotLostPlane) {
//							lostPlaneName = ""
//							definitelyInPlane() // once planeNotLostPlane is reset (e.g. by getting a flying None message) we run into this line
//						}
				}else{
debug("update plane name '"+planeName+"' to '"+what+"'")						
				  planeVerified = false
					lostPlaneName = ""
					planeName = what
					definitelyInPlane()
				}
				if(deathPause && planeNotEmpty) server.rules.warnDeath(Pilot.this.name, planeName, lastPlanePriceCommit, deathPauseUntil, invitations.value)
				else if(planeNotEmpty && planeNotLostPlane && ! planeVerified ) server.rules.warnPlane(Pilot.this.name, planeName, load, lastPlanePriceCommit, balance)
			}

			def returns(){
				commitPlanePrice()
				if(refund.value>0) {
					chat("Awarded a refund of "+refund.value+conf.names.currency+" for returning the "+planeName)
					balance () = server.rules.updateBalance(balance, refund)
					refund () = 0
				}
				persist()
				clear()
			} 
			def clear(){
				lostPlaneName = ""
				planeVerified = false
				flying=false
				planeName = ""
				planePrice = 0
				landed = true
				load = None
				lastBalance = None
			}
			def persist() {
//debug("persisting "+name)				
				server.balance .store(name, Some(balance(Armies.RedSide)), Some(balance(Armies.BlueSide)))
			}
			
		}
  
		def priceMessages(which:String, all:Boolean){
			val pilotName = Pilot.this.name
   
			server.planes.forMatches(which){plane =>		  
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
//debug("pilot "+name +" <- "+x)		    
				ImessageHandler isDefinedAt x
				
			}
			override def apply(x:Any) = {
			  if( ! conf.game.homeAlone.apply){
				  ImessageHandler.apply(x)
				}
      }
		}
				
		
		def ImessageHandler : PartialFunction[Any, Unit] = { 
			case Is.Persisted => state.persist()

			case Is.InPlaneForSide(plane, army) => {
				currentSide_=( army )
				state.updatePlaneName(plane)
			}
			

			case Is.Returning => {
				state.returns()
			}
			case Is.Dying => {
				state.dies()
			}
			case Is.Crashing => {
				state.crashes()
			}
			case inv : Pilots.Invitation => {
				invitations.put(inv.plane, inv)
			  if(state.deathPause) chat(inv.by + " invites you to fly "+inv.plane+" ("+((inv.until - System.currentTimeMillis)/1000)+"s)")
			}

			case Is.Loading(plane, load, _) => {
				state.updatePlaneName(plane)
				state.updateLoadout(load)
			}
			case Is.MissionEnd => {
				state.commitPlanePrice()
				state.clear()
			}
			case Is.MissionChanging => {
				state.clear()
			}
			case Is.MissionBegin => {
				state.clear()
			}
    	case Is.InFlight => {
    		
				if(state.somePlane){
debug(name + " Is.InFlight "+state) 
	    	  state.flying = true
	    	  state.commitPlanePrice()
				}else debug("ignored Is.InFlight for "+name)
    	}
      case Is.LandedAtAirfield => {
debug(name + " Is.LandedAtAirfield "+state) 
				state.returns()
    	}
 			case Is.Selecting => {
debug(name + " Is.Selecting "+state)    	  
				if(state.flying) {
					chat("refly counted as a lost plane")
					state.crashes()
				}else{
					state.clear()
				}
				
    	}
    	case Is.KIA => {
debug(name + " Is.KIA "+state)    	  
    	  state.dies()
    	}
    	case Is.HitTheSilk => {
debug(name + " Is.HitTheSilk "+state)    	  
				state.crashes()
    	}    
    	case Is.TakingSeat(plane) => {
debug(name + " Is.TakingSeat "+state)    	  
    		state.updatePlaneName(plane)
    	}
    	case Is.Joining => {
					state.lostPlaneName = ""
    	}
    	case Is.Leaving => {
    		if(state.flying) {
					 server.multi ! new server.multi.ChatBroadcast(this.name + " leaving, counted as a lost plane")
					state.crashes()
				}else{
					state.persist()
					state.clear()
				}
    	}

     
			case Is.Chatting(msg) => { 
debug(name + " sending chat "+msg)  			  
			  msg match { 
  	  		case Pilots.Commands.balancecommand(_) => {
						chat("current balance is "+balance)
					}
					case Pilots.Commands.pricecommand(which) => {
						priceMessages(which, true)
					}
					case Pilots.Commands.availablecommand(which) => {
						priceMessages(which, false)
					}
					case Pilots.Commands.statecommand(which) => {
						chat(state.toString)
					}
					case x => //debug("unknown command by "+name+":"+x)
			  }
			}
			
			case _ => unknownMessage _ 
		}
			

		private def chat(msg:String) = server.multi ! new server.multi.ChatTo(name, msg) 
		
	}
}
object Pilots {

	
	object Commands{
		val balancecommand = """(\s*!\s*balance\s*)""".r
		val pricecommand = """\s*!\s*price\s+(\S*)""".r
		val availablecommand = """\s*!\s*available\s+(\S*)""".r
		val statecommand = """\s*!\s*state\s*""".r
	}
	class Invitation(val by:String, val plane:IMarket.Loadout, val until:Long) 
}

