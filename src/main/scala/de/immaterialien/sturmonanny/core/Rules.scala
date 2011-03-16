package de.immaterialien.sturmonanny.core

import scala.collection.mutable
import net.liftweb.actor.LAPinger
import _root_.de.immaterialien.sturmonanny.util.Logging

class Rules extends NonUpdatingMember with Logging {
  import Rules._
  def startCost(price:Double):Double = math.max(0D, price * conf.game.startcost.apply)
  def startCost(price: Double, pilot:String, recruiter:Option[String], side:Armies.Armies, invitation:Option[AutoInvitations#InvitationState#Invitation]):PriceInfo = {
  	val cost:Double = startCost(price)
  	val list = if(cost==0D) {
			Nil 
		} else if(recruiter.isDefined){
  		val cruiterPart = recruiterPercents.toDouble/100D
  		val recruitPart = 1D-cruiterPart
  		Payment(pilot, cost*recruitPart)::Payment(recruiter.get, cost*cruiterPart)::Nil
  	}else{
  		Payment(pilot, cost)::Nil
  	}
  	PriceInfo(price, list,side, invitation)
  }
  
  def refund = (conf.game.refund.apply.toDouble / 100D) 
  def updateBalance(old: Double, diff: Double): Double = {
    val ret = old + diff
    val lowest = conf.pilots.lowestBalance.apply
    val highest = conf.pilots.highestBalance.apply
    
//debug("update balance "+old + " + "+diff)    
    if (ret < lowest) lowest
    else if (ret > highest) highest
    else ret
  }
  /**
   * @param players
   * @return length in seconds
   */
  def calculateDeathPause(players:Int): Int = {
  	val upper = conf.pilots.deathpenalty.apply
  	val step = conf.pilots.penaltyPerPilot.apply
  	val duration = if(step>upper) step else math.min(upper, step*players) 
    (duration)
  }
  def recruiterPercents = {
  	math.max(0, math.min(100, conf.recruiting.recruitshare.apply))
  }
  def recruitPercents = 100 - recruiterPercents
  def warnPlane(who: String, plane: String, load: Option[String], wouldCost:Double, since: Long, balance: Double, side:Armies.Armies) {
    val multi = server.multi
    val loadout = IMarket.Loadout(plane, load)
    //val startPrice = server.market.getPrice(loadout, side.id) * conf.game.startcost.apply
    
    var factor = conf.game.startcost.apply
//    val rawPrice = server.market.getPrice(loadout, side.id)
//    
//    val startPrice = rawPrice * factor
    val startPrice = wouldCost
//println(who+" warnPlane for "+loadout+ " @ "+side.id+" = "+startPrice)    
    if(startPrice > 0){ 
println(who+" startPrice>0")		    	
	    var difference = server.time.currentTimeMillis - since
	    val remaining = (conf.game.planeWarningsSeconds.apply * 1000) - difference
	    if (remaining < 0) {
println(who+" plane kick ")		    	
	      kick(who, "too much time in rare planes like " + loadout, 1000)
	      //			multi ! new multi.ChatBroadcast(who + " has been kicked: too much time in rare planes like "+loadout)
	    } else {
	      val seconds: Long = remaining / 1000
	
	      val ratio = remaining.toDouble / (remaining + difference).toDouble
println("ratio "+ratio+" from "+remaining+" diff "+difference+" after "+seconds)	      
	      val message = if (ratio > 0.8) {
	        who + ", please fly something more common than a " + loadout
	      } else if (ratio > 0.65) {
	        loadout + " requires " + startPrice + "% but you only have " + balance.toInt + conf.names.currency
	      } else if (ratio > 0.56) {
	        "You can check your " + conf.names.currency + " by chatting \"! balance\" to Server"
	      } else if (ratio > 0.48) {
	        "You can check plane prices by chatting \"! prices\" to Server"
	      } else if (ratio > 0.42) {
	        "Get out of that " + plane + ", " + who + ", we have other plans for it!"
	      } else if (ratio > 0.31) {
	        "You have " + seconds + "s to get out of that " + plane + "!"
	      } else if (ratio > 0.27) {
	        who + ", you are stealing a " + plane + "! You have " + seconds + "s!"
	      } else if (ratio > 0.22) {
	        "You have " + seconds + "s to get out of this " + plane + ", " + who + "!"
	      } else if (ratio > 0.19) {
	        "Are you a traitor, " + who + "? Get out of there ASAP!"
	      } else if (ratio > 0.12) {
	        "The " + plane + " must not fall into enemy hands!"
	      } else if (ratio > 0.08) {
	        "Opening fire on " + who + " in " + seconds + "s!"
	      } else {
	        who + ", if you want to live, jump!"
	      }
println(who+" plane warning "+message)	      
	      multi ! new multi.ChatTo(who, message)
	    }
    }
  }

  //	def kick(who : String, reason:String="unspecified reasons"){
  def kick(who: String, reason: String, remainingMillis : Long) {
    val cmdString: String = server.conf.game.kickCommand.apply
    val split = escapedNewline.split(cmdString)
    var timeAcc = 0
    /**
     * replace multiple variables while taking care to not match on replaced content 
     */
    def replaceVars(in: String): String = {
      in match {
        case headVarTail(h, "name", t) => h + who + replaceVars(t)
        case headVarTail(h, "qname", t) => h + "\"" + who + "\"" + replaceVars(t)
        case headVarTail(h, "reason", t) => h + reason + replaceVars(t)
        case _ => in
      }
    }
    
    val timedCommands = for (line <- split.toSeq) yield line match {
      case escapedNumber(numString, string) => {
        timeAcc = timeAcc + numString.toInt
        (timeAcc, replaceVars(string))
      }
      case text => (timeAcc, replaceVars(text))
    }

    var sumSecs = 0
    for ((secs, cmdText) <- timedCommands) {
    	sumSecs += secs
      val command = new server.multi.UpCommand(cmdText)
      if (secs == 0) server.multi ! command
      else LAPinger.schedule(server.multi, command, secs * 1000)
    }
    val remainInclMessages=math.max(sumSecs*1000, remainingMillis)
    
    server.pilots.forElement(who)(_! KickedUntil(server.time.now + remainInclMessages))
  }
  def warnDeath(who: String, what: String, pauseUntil: Long, pauseLen:Int, inviteString:Option[String], flying:Boolean) {
    val multi = server.multi
    val pauseDuration:Long = pauseLen
//    var difference = server.time.currentTimeMillis - since
//    val remaining = (pauseDuration * 1000) - difference
    
    val remaining = pauseUntil - server.time.now
    val difference = (pauseDuration*1000) - remaining 
    
    val seconds: Long = remaining / 1000
//    val pauseLen = conf.pilots.deathpenalty.apply

 
    if (what == null || what.trim.isEmpty) {

      multi ! new multi.ChatTo(who, who + ", you can fly " + inviteString.map(_ + " or wait ").getOrElse("again in ") + seconds + " seconds")
    } else {
      if (remaining < 0) {
        kick(who, "did not wait " + pauseLen + "s or for an invitation", remaining)
        //multi ! new multi.ChatBroadcast(who + " has been kicked: hit refly too fast")
      } else {
        val ratio = remaining.toDouble / (pauseDuration*1000).toDouble
        val message = if (ratio > 0.7) {
          if (inviteString.isDefined)
            "Fly " + inviteString.get + " or wait " + pauseLen + " seconds"
          else
//            "After dying, you are not allowed to fly for " + pauseLen + " seconds"
          	"current death pause: "+pauseLen+"s (depends on team size)" 
//        } else if (ratio > 0.4) {
//          if (inviteString.isDefined)
//            "Fly " + inviteString.get + " or wait " + pauseLen + " seconds"
//          else
//            who + ", you must be back in plane selection in " + seconds + " s!"
//        } else if (ratio > 0.2) {
//          "Kicking " + who + " in " + seconds + " seconds!"
//        } else {
//          who + ", if you want to live, jump!"
//        }
        }else{
          if (inviteString.isDefined)
            "Fly " + inviteString.get + " or wait " + pauseLen + " seconds"
          else
            who + ", you must not be in the air for " + seconds + " more seconds"
        }

        multi ! new multi.ChatTo(who, message)
      }
    }
  }
}
object Rules {
  private val escapedNewline = """\s*\\n\s*""".r
  private val escapedNumber = """\(\s*(\d+)\s*\)(.*)""".r
  private val headVarTail = """^(.*?)\$(name|qname|reason)(.*)$""".r
  case class CostResult(val allowed: Boolean, fullPayments : List[Payment])
  case class Payment(who:String, what:Double)
  case class KickedUntil(when:Long)
  
  case class PriceInfo(price:Double, payments:List[Payment], side:Armies.Armies, invitation:Option[AutoInvitations#InvitationState#Invitation]){
  	def forPilot(name:String)={
  		payments.find(_.who == name).map(_ what).getOrElse(0D)
  	}
  }
  val emptyPrice = PriceInfo(0D, Nil, Armies.None, None)
}