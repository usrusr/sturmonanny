package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

import net.liftweb.actor



class Pilots extends Domain[Pilots] with actor.LiftActor with NonUpdatingMember with Logging{ import Pilots._
	case class ClearToFly(pilot:Pilot, deathPauseUntil:Long)
	override def messageHandler = {
		case ClearToFly(pilot, deathPauseUntil) => if(deathPauseUntil == pilot.state.deathPauseUntil){
			server.multi ! new server.multi.ChatTo(pilot.name, "clear for takeoff")
		}
	}
	override def newElement(name:String) = new Pilot(name)
	class Pilot(override val name : String) extends Pilots.this.Element(name) with SideProvider{
		val balance = Army Var 0D
//		val refund = Army Var 0D
//		val invitations = Army Val (new mutable.HashMap[IMarket.Loadout, Pilots.Invitation]())
		
		for(loaded <- server.balance.load(name)){
			balance(Armies.RedSide) = loaded.red
			balance(Armies.BlueSide) = loaded.blue
			val currency = conf.names.currency
			chat("your balance: "+loaded.red.toInt+currency+" on red and "+loaded.blue.toInt+currency+" on blue")
		}
		
		val _invites = new AutoInvitations(this) 
		def invites = _invites.get // wrap each access in a timeout clean
		object state{
		  override def toString = {
		    " "+
		    (if(deathPauseUntil>System.currentTimeMillis) ("pause for "+(deathPauseUntil-System.currentTimeMillis)) else "") +
        (if(died) " died" else "") +
        (if(landed) " landed" else "") +
        " plane:" +planeName+" @ "+lastPayment + 
        (if(planeVerified) " verified" else " unchecked") +
        (if(lostPlaneName!="") " lost:"+lostPlaneName else "") +
        ""
		  }
		  
//			var deathPause = false
			var deathPauseUntil = 0L
			
			var died = false
			var landed = false
			var flying = false
			
			var planeName = ""
			var lastPlanePriceCommit = System.currentTimeMillis
			var planeVerified = true
			var lastPlaneVerification = 0L
			var lostPlaneName = ""
//			var planePrice : Double = 0
			var load : Option[String] = None
			//var lastBalance : Option[Double] = None
			
			/**
			 * full payment pilot->price, as divided after applying recruit share
			 */
			var lastPayment : Option[Rules.PriceInfo]=None 
			
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
			  if(planeVerified && deathPauseUntil<System.currentTimeMillis && somePlane) commitPlanePrice()// finish balance
			  val startFee = lastPayment.map(_ price).getOrElse(0D)

//				val result =server.rules.startCostCheck(planePrice, balance) match {
//				  case Rules.CostResult(false, _, _, cost) => {
//				    Some("you can't afford a "+planeName+" again")//+", chat \"! available\" for information")
//				  }
//				  case _ => None
//				}
			  lastPayment = None    
				lostPlaneName = planeName
				planeVerified = false
				planeName = ""
				
				load = None
				lastPayment.map(_ price).filter(_<balance).map{ _ =>
			  	Some("you can't afford a "+planeName+" again")//+", chat \"! available\" for information")
				} getOrElse None
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
			  if(planeVerified) for(planePrice <- lastPayment.map(_ price)){
println("commit plane price "+state)			  	
					val now = System.currentTimeMillis

					val millis = System.currentTimeMillis - lastPlanePriceCommit
					val difference = planePrice * millis / (-60000) 
					debug("price update for "+millis+" with raw price"+ currency(planePrice)+ " -> difference "+currency(difference) )       
					balance () = server.rules.updateBalance(balance, difference)
					lastPlanePriceCommit = now
					persist()
			  }
			}
			// call when it is definitely known that the pilot is fresh in a plane
			def definitelyInPlane() {
				lastPayment = None
				//invitations.value.retain(((x,y) => y.until > System.currentTimeMillis))
				
				
				val planePrice = server.market.getPrice(planeName, load)
				
				
				lostPlaneName = ""
				val now = System.currentTimeMillis
				lastPlanePriceCommit = now
				var invitation = invites in planeName
				val side = firstNonNeutral(invitation.map(_.inv.side))
				val rawPrice = server.rules.startCost(planePrice, name, invitation.map(_.inv.by), side, invitation)
				
//				if( ! invitation.accept(rawPrice))
				
				if(deathPauseUntil>now && invitation.isEmpty){
					// pilot is in death pause, invitations are checked in rules
					planeVerified = false
				} else {
					val myPrice = rawPrice.forPilot(name)
					
					
					if(myPrice>0D && myPrice>balance) {
						chat(""+myPrice+conf.names.currency+" needed, available "+(balance.value)+conf.names.currency+"")
						planeVerified = false
					}else{
						planeVerified = true
						lastPlaneVerification = System.currentTimeMillis
						for(i<-invitation) i.preaccept()
						pay(rawPrice, side, invitation)
					}
//					server.rules.startCostCheck(planePrice, balance) match {
//					case Rules.CostResult(true, newBalance, newRefund, startFee) => {
//							planeVerified = true
//							if(newBalance!=balance.value){
//								if(newRefund>0) chat("Start fee "+startFee+conf.names.currency+", possible refund: "+newRefund +conf.names.currency+"")
//								else chat("Start fee of "+startFee+conf.names.currency+" debited")
//							}
//							balance () = newBalance
//							
//						}
//					case Rules.CostResult(false, newBalance, newRefund, startFee) => {
//							chat(""+startFee+conf.names.currency+" needed, available "+(balance.value)+conf.names.currency+"")
//							
//							planeVerified = false
//						}
//					}
				}          
			}
			def planeNotEmpty = ! (planeName==null || planeName=="")
			def planeNotLostPlane = lostPlaneName==null || lostPlaneName=="" || planeName != lostPlaneName

			def pay(rawPrice:Rules.PriceInfo, side:Armies.Armies, invitation:Option[AutoInvitations#InvitationState#Invitation]) = {
				rawPrice.payments match {
					case me :: Nil => {
						chat("Start fee "+currency(me.what)++", possible refund: "+currency(refund(me.what)))
						balance (side) = server.rules.updateBalance(balance(side), - me.what) 
					}
					case me :: boss :: Nil => {
						chat("Start fee "+currency(me.what)++", possible refund: "+currency(refund(me.what)))
						balance (side) = server.rules.updateBalance(balance(side), - me.what) 
						domain.forElement(boss.who){ recruiter => 
							if(invitation.isDefined) recruiter ! BalanceUpdate(-boss.what, invitation.get.inv.side, " recruited "+name+", possible refund: "+currency(refund(boss.what)))
							else recruiter ! BalanceUpdate(-boss.what, currentSide, name+" used your invitation") // something's wrong if this message appears!
						}
					}
					case Nil => {
						chat("Plane verified")		
					}
					case other => for(pay<-other){
						domain.forElement(pay.who){ payer =>
							payer ! BalanceUpdate(
									-pay.what, 
									invitation.map(_.inv.side).getOrElse(currentSide), 
									" recruitment cost for "+name) // something's wrong if this message appears!
						}
					}
				}
			} 
			def firstNonNeutral(sides:Option[Armies.Armies]*):Armies.Armies={
				val fs = sides.flatten
				_firstNonNeutral(fs:_*)
			}
			def firstNonNeutral(sides:Armies.Armies*):Armies.Armies=_firstNonNeutral(sides:_*)
			private def _firstNonNeutral(sides:Armies.Armies*):Armies.Armies={
				val neutral:Armies.Armies = Armies.None
				val curr:Armies.Armies = currentSide
				if(curr != neutral) curr
				else sides.find(_!=neutral).getOrElse(neutral)
			}
			def updateLoadout(what:String){
				load = Some(what)
				if(planeVerified){
					val newPrice = server.market.getPrice(planeName, load)
					val inv = invites.current
					//	  					val invitation = invites.current
//				val side = firstNonNeutral(invitation.map(_.inv.side))

					val side = firstNonNeutral(inv.map(_.inv.side))
					val newPriceInfo = server.rules.startCost(newPrice, name, inv.map(_.inv.by),side, inv)
					if(lastPayment.isEmpty) {
						chat("directly paying with loadout...")
						pay(newPriceInfo, side, inv)
					} else { 
						val payment = lastPayment.get
						if(payment.price != newPrice ){
							if(inv.isDefined){
								if(inv.get.inv.priceLimit<newPrice){
									val diff = newPrice - payment.price  // positive if more expensive
									chat("expensive loadout not paid for by recruiter: -"+currency(diff))
									balance(side) = server.rules.updateBalance(balance(side), -diff) 
								}else{
									for(newpm<- newPriceInfo.payments){
										val old = payment.forPilot(newpm.who)
										
										val thisDiff = newpm.what - old // positive if more expensive
										if(newpm.who == name) {
											chat("Loadout update "+currency(thisDiff)+", total possible refund: "+currency(refund(newpm.what)))
											balance (side) = server.rules.updateBalance(balance(side), - thisDiff) 
										}else{
											domain.forElement(newpm.who){ recruiter =>
												recruiter ! BalanceUpdate(-thisDiff, side, "Loadout update for recruit "+name+" "+currency(thisDiff)+", total possible refund: "+currency(refund(newpm.what)))
											}
										}
									}
								}
							}else{
								// no invite
								val diff = newPrice - payment.price  // positive if more expensive
								chat("loadout update: -"+currency(diff))
								balance(side) = server.rules.updateBalance(balance(side), -diff)
							}
						}
						lastPayment = Some(newPriceInfo)
					}
				}
			}
			def updatePlaneName(what:String){
				if(what==null || what.trim.isEmpty){
					planeVerified = false
					planeName = ""
					lostPlaneName = ""
					lastPayment = None
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
				if(deathPauseUntil>System.currentTimeMillis && planeNotEmpty) server.rules.warnDeath(Pilot.this.name, planeName, lastPlanePriceCommit, deathPauseUntil, invites.allInvitationsLine)
				else if(planeNotEmpty && planeNotLostPlane && ! planeVerified ) server.rules.warnPlane(Pilot.this.name, planeName, load, lastPlanePriceCommit, balance)
			}

			def returns(){
				commitPlanePrice()
				for(pi<-lastPayment; pay<-pi.payments ){
					val ref = refund(pay.what)
					if(pay.who==name){
						balance(pi.side) = server.rules.updateBalance(balance(pi.side), ref)
						chat("ref: "+currency(ref))
					}else{
						domain.forElement(pay.who){ recruiter =>
							recruiter ! BalanceUpdate(ref, pi.side, "Refund for recruit "+name+": "+currency(ref))
						}
					}
				}
//				if(refund.value>0) {
//					chat("Awarded a refund of "+refund.value+conf.names.currency+" for returning the "+planeName)
//					balance () = server.rules.updateBalance(balance, refund)
//					refund () = 0
//				}
				persist()
				clear()
			} 
			def clear(){
				lostPlaneName = ""
				planeVerified = false
				flying=false
				planeName = ""
				lastPayment = None
				landed = true
				load = None
				invites.clean
			}
			def persist() {
//debug("persisting "+name)				
				server.balance .store(name, Some(balance(Armies.RedSide)), Some(balance(Armies.BlueSide)))
			}
			
		}
  
		def priceMessages(which:String, all:Boolean){
			val pilotName = Pilot.this.name
   
			server.planes.forMatches(which){plane =>		  
			/**
			 * todo: by-loadout price info!
			 */
				val price = server.market.getPrice(plane.name)

				def padRight(in:String, reference:String):String=in+(reference.drop(in.length))
				def padLeft(in:String, reference:String):String=(reference.drop(in.length))+in
				val (result, affordable, verb) = (if(price > 0){
					
					val cost = server.rules.startCost(price)
					val bal:Double=balance
					val affordable = cost < bal 
					
					if(affordable){
						(true, "+", "costs.once."+cost.toInt+".+" )
					}else{
						(false, "!", "would.cost."+cost.toInt+".+" )
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
				
		def refund(in:Double)={
			in * server.rules.refund
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
			case inv : Invite => {
				invites add inv
			}

			case Is.Loading(plane, load, _) => {
				state.updatePlaneName(plane)
				state.updateLoadout(load)
			}
			case Is.MissionEnd => {
				state.returns()
				state.clear()
			}
			case Is.MissionChanging => {
				state.returns()
				state.clear()
			}
			case Is.MissionBegin => {
				state.returns()
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
					state.returns()
					state.clear()
				}
				joinNeutral()
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
    	case BalanceUpdate(diff, side, reason)=>{
    		val before = balance(side)
    		val after = server.rules.updateBalance(balance(side), diff)
    		balance(side) = after
    		val color = if(side==currentSide) "" else " ("+side+")" 
    		if(reason.isEmpty) chat("updated balance"+color+" from "+currency(before)+ " to "+currency(after))
    		else chat(currency(diff)+ color+": "+reason)
    	}
    	case i:Invite=>{
    		invites add i 
    	}
     
			case Is.Chatting(msg) => { 
debug(name + " sending chat "+msg)  			  
			  msg match { 
  	  		case Commands.balance(_) => {
						chat("current balance is "+balance)
					}
					case Commands.price(which) => {
						priceMessages(which, true)
					}
					case Commands.available(which) => {
						priceMessages(which, false)
					}
					case Commands.state(which) => {
						chat(state.toString)
					}
					case Commands.recruit(who) => {
						val secs = conf.recruiting.time.apply 
						if( ! state.planeVerified){
							chat("You can't recruit outside a plane")
						}else if(invites.current.isDefined){
							chat("You are a recruit yourself")
						}else if(state.lastPlaneVerification + (secs * 1000) < System.currentTimeMillis){
							chat("You can only recruit for up to "+secs+" s after starting")
						}else{
							val ni = Invite(
													name, 
													IMarket.Loadout(state.planeName, state.load), 
													System.currentTimeMillis+(1000*secs), 
													currentSide,
													state.lastPayment.map(_ price).getOrElse(0D)
							)
							domain.forMatches(who){recruit=>
								recruit ! ni
							}
						}
					}
					
					case Commands.invites(_) => {
						val i = invites
						chat(i.allInvitationsLine.getOrElse("no invites"))
					}
					case Commands.help(_) => {
						chat("available commands are:")
						chat("help [command], balance, price [plane], available [plane], recruit [pilot], invites", 500)
					}					
					case Commands.helpCommand(cmd)=> cmd match {
						case "balance"=>chat("displays how many "+conf.names.currency+" you have")
						case "available"=>chat("'available abc' displays the allowed planes with 'abc' in their name")
						case "price"=>chat("'price abc' displays the price of planes with 'abc' in their name")
						case "state"=>chat("shows info about your current state")
						case "recruit"=>{
							chat("'recruit abc' invites pilots with 'abc' in their name to join your flight")
							chat("you can recruit for up to "+conf.recruiting.time.apply+" s after starting"
									, 1000)
							val planeDesc = if(state.planeName!=null && ! state.planeName.isEmpty) "a "+state.planeName else "the same plane as you"
							chat("the invitation will be accepted if the other pilot takes "+ planeDesc + " within that timespan"
									,2000)
							chat("death-timeout is skipped for your recruits"
									,3000)
							val percs = server.rules.recruiterPercents
							chat("you will pay "+percs+"% of your recruit's takeoff fee and get "+percs+"% of his landing refund"
									,4000)
						}
						case "invites"=>chat("'invites' displays recruitment invitations")
					}
			
					case x => //debug("unknown  by "+name+":"+x)
			  }
			}
			
			case _ => unknownMessage _ 
		}
			

		def chat(msg:String, delay : Long = 0) {
			val m = new server.multi.ChatTo(name, msg)
			if(delay<1) server.multi ! m
			else net.liftweb.actor.LAPinger.schedule(server.multi, m, delay)
		}
		def currency(double:Double):String={
			("%1.0f" format double)+conf.names.currency.apply
		}
	}
}
object Pilots {
	case class BalanceUpdate(diff:Double, side:Armies.Armies, reason:String="")
	
	object Commands{
		val balance = """(\s*!\s*balance\s*)""".r
		val price = """\s*!\s*price\s+(\S*)""".r
		val available = """\s*!\s*available\s+(\S*)""".r
		val state = """\s*!\s*state\s*""".r
		val recruit = """\s*!\s*recruit\s+(\S*)""".r
		val invites = """\s*!\s*invites\s*""".r
		val help = """\s*!\s*help\s*""".r
		val helpCommand = """\s*!\s*help\s+(\S*)""".r
	}
//	class Invitation(val by:String, val plane:IMarket.Loadout, val until:Long) 
}

