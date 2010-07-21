package de.immaterialien.sturmonanny.core

import scala.collection.mutable
import de.immaterialien.sturmonanny.util.Logging
 
class Rules extends NonUpdatingMember with Logging { 
	def startCost(price:Double) = price * conf.game.startcost 
	def startCostCheck(price:Double, balance:Double) :Rules.CostResult = {
		if(price>0){
			val cost = startCost(price)
			val newBalance = balance - cost
			if(newBalance > 0) {
				Rules.CostResult(true, updateBalance(balance, -cost), 100 * cost / conf.game.refund, cost)
			} else {
				Rules.CostResult(false, balance, 0, cost)  
			}
		}else{
			// jumping in a negatively priced plane does not immediately change the balance
			Rules.CostResult(true, balance, 0, 0)
			
		}
	} 
	def updateBalance(old:Double, diff:Double) : Double = {
		val ret = old + diff
		val lowest = conf.pilots.lowestBalance.toDouble
		val highest = conf.pilots.highestBalance.toDouble
		if(ret<lowest) lowest
		else if(ret>highest) highest 
		else ret
	}
	def calculateDeathPause : Long = {
		System.currentTimeMillis + (1000 * conf.pilots.deathpenalty)
	}
 
	def warnPlane(who:String, what:String, since:Long, balance:Double){
		val multi = server.multi
		var difference = System.currentTimeMillis - since 
		val remaining = ( conf.game.planeWarningsSeconds * 1000 ) - difference
		if(remaining < 0) {
//			kick(who)
			multi ! new multi.ChatBroadcast(who + " has been kicked: too much time in rare planes like "+what)
		}else{
			lazy val startPrice = server.market.getPrice(what) * conf.game.startcost
			val seconds : Long = remaining / 1000

			val ratio = remaining.toDouble/(remaining+difference).toDouble
			val message = if(ratio>0.8) {
				who+", please fly something more common than a "+what
			}else if(ratio>0.65){
				what+" requires "+startPrice+"% but you only have "+balance+"%"
			}else if(ratio>0.56){
				"You can check your "+conf.names.currency+" by chatting \"! balance\" to Server"
			}else if(ratio>0.48){
				"You can check plane prices by chatting \"! prices\" to Server"
			}else if(ratio>0.42){
				"Get out of that "+what+", "+who+", we have other plans for it!"
			}else if(ratio>0.31){
				"You have "+seconds+"s to get out of that "+what+"!"
			}else if(ratio>0.27){
				who+ ", you are stealing a "+what+"! You have "+seconds+"s!"
			}else if(ratio>0.22){
				"You have "+seconds+"s to get out of this "+what+", "+who+"!"
			}else if(ratio>0.19){
				"Are you a traitor, "+who+"? Get out of there ASAP!"
			}else if(ratio>0.12){
				"The "+what+" must not fall into enemy hands!"
			}else if(ratio>0.08){
				"Opening fire on "+who+" in "+seconds+"s!"
			}else{
				who+", if you want to live, jump!"
			}
			multi ! new multi.ChatTo(who, message)
		}
	}
	def kick(who : String){
	  if(true) debug("NOT KICKING "+who)else 
	  server.multi ! new server.multi.Kick(who)
   }
	def warnDeath(who:String, what:String, since:Long, pauseUntil:Long, invitations:mutable.Map[String, Pilots.Invitation] ){
		val multi = server.multi
		var difference = System.currentTimeMillis - since 
		val remaining = ( conf.game.planeWarningsSeconds * 1000 ) - difference
		val seconds : Long = remaining / 1000
		
		val inviteString = invitations.values.toList match{
		case Nil => None
		case first :: Nil => Some(first.plane +" (with "+first.by+")")
		case x => Some(x.map(_.plane).mkString(", "))
		}
		
		if(what==null||what.trim.isEmpty){
			
			multi ! new multi.ChatTo(who, who+", you can fly "+inviteString.map(_+" or wait ").getOrElse("again in ")+seconds+" seconds")
		}else{
			if(remaining < 0) {
				kick(who)
				multi ! new multi.ChatBroadcast(who + " has been kicked: hit refly too fast")
			}else{
				val ratio = remaining.toDouble/(remaining+difference).toDouble
				val message = if(ratio>0.7) {
					if(inviteString.isDefined) 
					"Fly "+inviteString.get+" or wait "+seconds+" seconds"
					else 
					"After dying, you are not allowed to fly for "+seconds+" seconds"
				}else if(ratio>0.4){
					if(inviteString.isDefined) 
					"Fly "+inviteString.get+" or wait "+seconds+" seconds"
					else 
					who+", you must be back in plane selection in "+seconds+" s!"
				}else if(ratio>0.2){
					"Kicking "+who+" in "+seconds+" seconds!"
				}else{
					who+", if you want to live, jump!"
				}
				
				multi ! new multi.ChatTo(who, message)
			}
		}
	}
} 
object Rules {
case class CostResult(val allowed:Boolean, newBalance : Double, refund : Double, startFee:Double)
}