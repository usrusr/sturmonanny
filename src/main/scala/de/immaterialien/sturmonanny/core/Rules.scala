package de.immaterialien.sturmonanny.core

import scala.collection.mutable
import net.liftweb.actor.LAPinger
import _root_.de.immaterialien.sturmonanny.util.Logging

class Rules extends NonUpdatingMember with Logging {
  import Rules._
  def startCost(price: Double) = price * conf.game.startcost.apply
  def startCostCheck(price: Double, balance: Double): Rules.CostResult = {
    if (price > 0) {
      val cost = startCost(price)
      val newBalance = balance - cost
      if (newBalance > 0) {
        Rules.CostResult(true, updateBalance(balance, -cost), 100 * cost / conf.game.refund.apply, cost)
      } else {
        Rules.CostResult(false, balance, 0, cost)
      }
    } else {
      // jumping in a negatively priced plane does not immediately change the balance
      Rules.CostResult(true, balance, 0, 0)

    }
  }
  def updateBalance(old: Double, diff: Double): Double = {
    val ret = old + diff
    val lowest = conf.pilots.lowestBalance.apply
    val highest = conf.pilots.highestBalance.apply
    if (ret < lowest) lowest
    else if (ret > highest) highest
    else ret
  }
  def calculateDeathPause: Long = {
    System.currentTimeMillis + (1000 * conf.pilots.deathpenalty.apply)
  }

  def warnPlane(who: String, plane: String, load: Option[String], since: Long, balance: Double) {
    val multi = server.multi
    var difference = System.currentTimeMillis - since
    val remaining = (conf.game.planeWarningsSeconds.apply * 1000) - difference
    val loadout = IMarket.Loadout(plane, load)
    if (remaining < 0) {
      kick(who, "too much time in rare planes like " + loadout)
      //			multi ! new multi.ChatBroadcast(who + " has been kicked: too much time in rare planes like "+loadout)
    } else {
      lazy val startPrice = server.market.getPrice(loadout) * conf.game.startcost.apply
      val seconds: Long = remaining / 1000

      val ratio = remaining.toDouble / (remaining + difference).toDouble
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
      multi ! new multi.ChatTo(who, message)
    }
  }

  //	def kick(who : String, reason:String="unspecified reasons"){
  def kick(who: String, reason: String) {
    val cmdString: String = server.conf.game.kickCommand
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
    for ((secs, cmdText) <- timedCommands) {
      val command = new server.multi.UpCommand(cmdText)
      if (secs == 0) server.multi ! command
      else LAPinger.schedule(server.multi, command, secs * 1000)
    }
  }
  def warnDeath(who: String, what: String, since: Long, pauseUntil: Long, invitations: mutable.Map[IMarket.Loadout, Pilots.Invitation]) {
    val multi = server.multi
    var difference = System.currentTimeMillis - since
    val pauseDuration = conf.game.planeWarningsSeconds.apply
    val remaining = (pauseDuration * 1000) - difference
    val seconds: Long = remaining / 1000

    cleanInvitations(invitations)
    val inviteString = invitations.values.toList match {
      case Nil => None
      case first :: Nil => Some(first.plane + " (with " + first.by + ")")
      case x => Some(x.map(_.plane).mkString(", "))
    }

    if (what == null || what.trim.isEmpty) {

      multi ! new multi.ChatTo(who, who + ", you can fly " + inviteString.map(_ + " or wait ").getOrElse("again in ") + seconds + " seconds")
    } else {
      if (remaining < 0) {
        kick(who, "did not wait " + pauseDuration + "s or for an invitation")
        //multi ! new multi.ChatBroadcast(who + " has been kicked: hit refly too fast")
      } else {
        val ratio = remaining.toDouble / (remaining + difference).toDouble
        val message = if (ratio > 0.7) {
          if (inviteString.isDefined)
            "Fly " + inviteString.get + " or wait " + seconds + " seconds"
          else
            "After dying, you are not allowed to fly for " + seconds + " seconds"
        } else if (ratio > 0.4) {
          if (inviteString.isDefined)
            "Fly " + inviteString.get + " or wait " + seconds + " seconds"
          else
            who + ", you must be back in plane selection in " + seconds + " s!"
        } else if (ratio > 0.2) {
          "Kicking " + who + " in " + seconds + " seconds!"
        } else {
          who + ", if you want to live, jump!"
        }

        multi ! new multi.ChatTo(who, message)
      }
    }
  }
}
object Rules {
	def cleanInvitations(invitations:mutable.Map[IMarket.Loadout, Pilots.Invitation])=invitations.retain(((x,y) => y.until > System.currentTimeMillis))
  private val escapedNewline = """\s*\\n\s*""".r
  private val escapedNumber = """\(\s*(\d+)\s*\)(.*)""".r
  private val headVarTail = """^(.*?)\$(name|qname|reason)(.*)$""".r
  case class CostResult(val allowed: Boolean, newBalance: Double, refund: Double, startFee: Double)
}