package de.immaterialien.sturmonanny.core

class Rules extends NonUpdatingMember {
  def startCostCheck(plane:String, balance:Double) :Rules.CostResult = {
    val price = server.market.getPrice(plane)
    if(price>0){
    	val startCost = price * conf.game.startcost
	    val newBalance = balance - startCost
	    if(newBalance > 0) {
	      Rules.CostResult(true, updateBalance(balance, startCost), 100 * startCost / conf.game.refund)
	    } else {
	      Rules.CostResult(false, balance, 0)  
	    }
    }else{
      // jumping in a negatively priced plane does not immediately change the balance
      Rules.CostResult(true, balance, 0)
      
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

  def warnPlane(who:String, what:String, since:Long, balance:Double){
    val multi = server.multi 
    var difference = System.currentTimeMillis - since 
    difference = if(difference==0) 1 else difference
    val remaining = ( conf.game.planeWarningsSeconds * 1000 ) - difference
    if(remaining < 0) {
      multi ! multi.Kick(who) 
      multi ! multi.ChatBroadcast(who + " has been kicked: too much time in rare planes like "+what)
    }else{
      lazy val startPrice = server.market.getPrice(what) * conf.game.startcost
      val seconds : Long = remaining / 1000

      val ratio = remaining+difference/difference
      val message = if(ratio>0.8) {
        who+", please fly something more common than a "+what
      }else if(ratio>0.65){
        what+" requires "+startPrice+"% but you only have "+balance+"%"
      }else if(ratio>0.56){
        "You can check your %s by chatting \"! balance\" to Server"
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
      multi ! multi.ChatTo(who, who+", "+message)
    }
  }
} 
object Rules {
    case class CostResult(val allowed:Boolean, newBalance : Double, refund : Double)
}