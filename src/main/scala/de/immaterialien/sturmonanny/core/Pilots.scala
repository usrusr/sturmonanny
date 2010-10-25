package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._

import net.liftweb.actor



class Pilots extends Domain[Pilots] with actor.LiftActor with NonUpdatingMember with Logging{ 
	import Pilots._

	
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
		
		var verbose = true
		
		
		
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
			
			
			var died = true
			var crashed = true 
			var landed = true
			
			var flying = false

			var planeName = ""
			var lastPlanePriceCommit = System.currentTimeMillis
			var planeWarningSince = 0L
			var lastCleared = 0L
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
				if( ! died) {
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
				}
				flying=false
				died = true
			}
			private def planeLost() : Option[String] = {
			  if(planeVerified && deathPauseUntil<System.currentTimeMillis && somePlane) commitPlanePrice()// finish balance
				flying=false
			  val startFee = lastPayment.map(_ price).getOrElse(0D)

//				val result =server.rules.startCostCheck(planePrice, balance) match {
//				  case Rules.CostResult(false, _, _, cost) => {
//				    Some("you can't afford a "+planeName+" again")//+", chat \"! available\" for information")
//				  }
//				  case _ => None
//				}
			  lastPayment = None    
				lostPlaneName = planeName
				unverify()
				planeName = ""
				
				load = None
				lastPayment.map(_ price).filter(_<balance).map{ _ =>
			  	Some("you can't afford a "+planeName+" again")//+", chat \"! available\" for information")
				} getOrElse None
			}
			def crashes() = {
				if( ! crashed){
					val priceMsg = planeLost()
	    
					priceMsg.map{msg => 
					  chat(name+": "+msg+", chat \"! available\"")
					}
					flying=false
				}
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
			
			/**
			 * use to filter outdated negative news that might still trickle in for a while after  
			 * 
			 * @return
			 */
			def notFresh = 10000L > System.currentTimeMillis - math.max(lastCleared, lastPlaneVerification)
			
			// call when it is definitely known that the pilot is fresh in a plane
			def definitelyInPlane() {
				lastPayment = None
println("    definitelyInPlane  ")
				//invitations.value.retain(((x,y) => y.until > System.currentTimeMillis)) 
				
				
				val planePrice = server.market.getPrice(planeName, load, currentSide.id)
				
				
				lostPlaneName = ""
				val now = System.currentTimeMillis
				lastPlanePriceCommit = now
				var invitation = invites in planeName
				val side = firstNonNeutral(invitation.map(_.inv.side).toList:_*)
				val rawPrice = server.rules.startCost(planePrice, name, invitation.map(_.inv.by), side, invitation)
				
//				if( ! invitation.accept(rawPrice))
				
				if(deathPauseUntil>now && invitation.isEmpty){
					// pilot is in death pause, invitations are checked in rules
					unverify()
				} else {
					val myPrice = rawPrice.forPilot(name)
					
					
					if(myPrice>0D && myPrice>balance) {
						chat(""+myPrice+conf.names.currency+" needed, available "+(balance.value)+conf.names.currency+"")
						unverify()
					}else{
						verify()
						died = false
						crashed = false
						landed = false
						
						lastPlaneVerification = System.currentTimeMillis
						for(i<-invitation) i.preaccept()
						pay(rawPrice, side, invitation)
					}
//					server.rules.startCostCheck(planePrice, balance) match {
//					case Rules.CostResult(true, newBalance, newRefund, startFee) => {
//							verify()
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
//							unverify()
//						}
//					}
				}          
			}
			def planeNotEmpty = ! (planeName==null || planeName=="")
			def planeNotLostPlane = lostPlaneName==null || lostPlaneName=="" || planeName != lostPlaneName

			def pay(rawPrice:Rules.PriceInfo, side:Armies.Armies, invitation:Option[AutoInvitations#InvitationState#Invitation]) {
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
						state.verify()
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
debug("setting price to in pay "+rawPrice, new Exception())				
				lastPayment = Some(rawPrice)
			} 
			def firstNonNeutralOption(sides:Option[Armies.Armies]*):Armies.Armies={
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
error("this code should be dead code now!")					
					val newPrice = server.market.getPrice(planeName, load, currentSide.id)
					val inv = invites.current
					//	  					val invitation = invites.current
//				val side = firstNonNeutral(invitation.map(_.inv.side))

					val side = firstNonNeutral(inv.map(_.inv.side).toList:_*)
					val newPriceInfo = server.rules.startCost(newPrice, name, inv.map(_.inv.by),side, inv)
					if(lastPayment.isEmpty) {
						if(verbose) chat("directly paying with loadout "+what)
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
debug("setting price to in updateLoadout "+newPriceInfo)						
						lastPayment = Some(newPriceInfo)
					}
				}
			}
			def unverify() {
				planeVerified = false
//warn("unverifying ", new Exception)				
			}
			def verify() {
				planeVerified = true
//warn("verifying ", new Exception)				
			}
			
			def planeVerifiable = ! planeVerified && currentSide != Armies.None && planeName != "" && load.isDefined
			def warnPlane(){
				if( ! planeVerified &&  planeVerifiable && planeNotEmpty && planeNotLostPlane) {
					if(planeWarningSince == 0) planeWarningSince = System.currentTimeMillis
					server.rules.warnPlane(Pilot.this.name, planeName, load, planeWarningSince, balance, currentSide)
				}
			}
			def tryPlaneVerification() {
				if( planeVerifiable ) {
					definitelyInPlane()
					if( ! planeVerified) server.rules.warnPlane(Pilot.this.name, planeName, load, lastPlanePriceCommit, balance, currentSide)					
				}
			}
			
			def updatePlaneName(what:String){
				if(what==null || what.trim.isEmpty){
					unverify()
					planeName = ""
					lostPlaneName = ""
					lastPayment = None
				}else if(what==planeName) { // plane name did not change
//						if(( ! planeVerified) && planeNotLostPlane) {
//							lostPlaneName = ""
//							definitelyInPlane() // once planeNotLostPlane is reset (e.g. by getting a flying None message) we run into this line
//						}
					tryPlaneVerification()
				}else{
debug("update plane name '"+planeName+"' to '"+what+"'")						
				  unverify()
					lostPlaneName = ""
					planeName = what
					tryPlaneVerification()
				}
				if(deathPauseUntil>System.currentTimeMillis && planeNotEmpty) server.rules.warnDeath(Pilot.this.name, planeName, lastPlanePriceCommit, deathPauseUntil, invites.allInvitationsLine)
				else warnPlane()
			}

			def returns(){
				if(flying && ! landed){
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
					landed = true
					persist()
					clear()
				}
			} 
			def clear(){
				lostPlaneName = ""
				unverify()
				flying=false
				planeName = ""
				lastPayment = None
				load = None
				invites.clean
				joinNeutral()
				planeWarningSince = 0L
				lastCleared = System.currentTimeMillis
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
				val price = server.market.getPrice(plane.name, currentSide.id)

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
//				ImessageHandler isDefinedAt x
				sourceFilteredMessageHandler isDefinedAt x
			}
			override def apply(x:Any) = {
			  if( ! conf.game.homeAlone.apply){
debug("pilot "+Pilot.this.name + " <- "+x)			  	
//				  ImessageHandler apply x
				  sourceFilteredMessageHandler apply x
				}
      }
		}
				
		def refund(in:Double)={
			in * server.rules.refund
		}
		var lastConsoleEvent : Is.Event = Is.Unknown
		var lastConsoleUserState : Is.Event = Is.Unknown
		var lastLogEvent : Is.Event = Is.Unknown
		lazy val sourceFilteredMessageHandler : PartialFunction[Any, Unit] = {
			case EventSource.Console(EventSource.UserState(event)) => {
				if(event == lastConsoleUserState) {
//debug("ignoring user state "+event+" because of repetition")					
				}else{
					ImessageHandler.apply(event)
					if(event != Is.InFlight) { // Is.InFlight is always allowed, never blocked 
debug("memorizing for repetition check user state event "+event)					
						lastConsoleUserState = event
					}
				}
			}


			case EventSource.Console(event) => {
				if(event == lastConsoleEvent) {
debug("ignoring console "+event+" because of repetition")					
				}else{
					ImessageHandler.apply(event)
					if(event != Is.InFlight) { // Is.InFlight is always allowed, never blocked 
debug("memorizing  for repetition check console event "+event)					
						lastConsoleEvent = event
					}
				}
			}
			case EventSource.Logfile(event) => {
					if(event == lastLogEvent) {
debug("ignoring log "+event+" because of repetition")					
				}else{
					ImessageHandler.apply(event)
debug("memorizing  for repetition check log event "+event)					
					lastLogEvent = event
				}
			}
			case x => ImessageHandler.apply(x)
		}
		
		lazy val ImessageHandler : PartialFunction[Any, Unit] = { 
			case Is.Persisted => state.persist()

			case Is.InPlaneForSide(plane, army) => {
				currentSide_=( army )
				state.updatePlaneName(plane)
			}
			

			case Is.Returning => {
				state.returns()
			}
			case Is.Dying => if(state.notFresh){
				state.dies()
			}
			case Is.Crashing => if(state.notFresh){
				state.crashes()
			}
			case inv : Invite => {
				invites add inv
			}

			case Is.Loading(plane, load, _) => {
				
				state.updateLoadout(load)
				state.updatePlaneName(plane)
			}
			case Is.MissionEnd => {
				state.returns()
			}
			case Is.MissionChanging(_) => {
				state.returns()
			}
			case Is.MissionBegin => {
				state.returns()
			}
    	case Is.InFlight => {
    		
				if(state.planeVerified){
debug(name + " Is.InFlight "+state) 
	    	  state.flying = true
	    	  state.commitPlanePrice()
				}else if(state.planeVerifiable) {
					state.warnPlane()
				}
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
					//state.clear()
				}
				state.clear()
    	}
    	case Is.KIA => if((state.flying||state.crashed) && state.notFresh){
debug(name + " Is.KIA "+state)    	  
    	  state.dies()
    	}
    	case Is.HitTheSilk => if(state.flying && state.notFresh){
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
     
			case Is.Chatting(msg) => processCommand(msg)
			
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
		def processCommand(msg:String){ 
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
					case Commands.state(_) => {
						chat("state:")
						chat( (if(state.planeVerified)"verified" else "unchecked" )+state.toString)
					}
					case Commands.recruit(who) => {
						val secs = conf.recruiting.time.apply 
						if( ! state.planeVerified){
							chat("You can't recruit outside a plane")
							if(verbose) chat("state is: "+state.toString, 300)
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
					case Commands.verbose(_) => {
						verbose = ! verbose
						chat("verbose -> " + (if(verbose) "on" else "off"))
						
					}		
//					case Commands.help(_) => {
//						chat("available commands are:")
//						chat("help [command], balance, price [plane], available [plane], recruit [pilot], invites", 500)
//					}
					case Commands.helpCommand(cmd)=> cmd.trim match {
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
						case "verbose"=>chat("toggles between verbose (debug) mode and a less chatty nanny")
						case _ => {
							chat("available commands are:")
							chat("help [command], balance, price [plane], available [plane], recruit [pilot], invites", 500)							
						}
					}
			
					case x => debug("unknown  by "+name+":"+x)
			  }
			}		
		
		
		
//		case class PilotState( 
//				died : Todo = Done(false), 
//				crashed : Todo = Done(false), 
//				flying : Todo = Done(false), 
////				landed : Todo = Done(true),
//				verified:Boolean = false, 
//				plane:String = "", 
//				load:String = "", 
//				side:Armies.Armies = Armies.None 
//				
//		) {
//			def updated : PilotState = {
//			if(died.unhandledTrue){
//				state.dies
//				//copy(died = died.check, flying = Done(false), crashed = Done(true), plane = "", load="")
//				PilotState(died = died.check, flying = Done(false), crashed = Done(true))
//			}else if(crashed.unhandledTrue || flying.unhandledFalse){
//				state.crashes
//				copy(flying = Done(false), crashed = Done(true))
//			}else if(plane!="" && side!=Armies.None && flying.todo){
//				this
//			}else{
//				this
//			}
//		}
//		def withPlane(np:String)={
//			if(np==plane) this
//			else this.copy(flying=Todo(false), plane=np, load="")
//		}.updated
//		def withLoad(nl:String)={
//			if(nl==load) this
//			else this.copy(flying=Todo(false), load=nl)
//		}.updated
//		def withSide(na:Armies.Armies)={
//			if(na==side) this
//			else if(flying.handledTrue) {
//				this.copy(side=na, flying = Todo(false))
//			}else{
//				this.copy(side=na)
//			}
//		}.updated
//		
//}
//	val x = new PilotState() withPlane "test"
//		
	}
}
object Pilots {

//	def Done(v: Boolean)=Todo(v, false)
//	/**
//	 * for substates that should be processed exactly once after getting updated, 
//	 * but might not be consumed at once (if the processing requires some other substates)  
//	 * @author ulf
//	 */
//	case class Todo(value:Boolean, todo:Boolean=true){
//		def done = ! todo
//		def unhandledTrue = todo && value 
//		def unhandledFalse = todo && ! value 
//		def handledTrue = ( ! todo) && value 
//		def handledFalse = ( ! todo) && ! value 
//		def check = if(todo) this.copy(todo=false) else this
//		def uncheck = if(todo) this else this.copy(todo=true) 
//	}	
//	
	case class BalanceUpdate(diff:Double, side:Armies.Armies, reason:String="")
	
	object Commands{
		
		
		
		val balance = """(?i-)\s*!\s*balance\s*(\s\S*)?""".r
		val price = """(?i-)\s*!\s*price\s*(\s\S*)?""".r
		val available = """(?i-)\s*!\s*available\s*(\s\S*)?""".r
		val state = """(?i-)\s*!\s*state\s*(\s\S*)?""".r
		val recruit = """(?i-)\s*!\s*recruit\s*(\s\S*)?""".r
		val invites = """(?i-)\s*!\s*invites\s*""".r
//		val help = """(?i-)\s*!\s*help(\s*)""".r
		val helpCommand = """(?i-)\s*!\s*help\s*(\s\S*)?""".r
		val verbose = """(?i-)\s*!\s*verbose\s*(\s\S*)?""".r
	}
//	class Invitation(val by:String, val plane:IMarket.Loadout, val until:Long) 
}

