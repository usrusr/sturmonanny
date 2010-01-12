package de.immaterialien.sturmonanny.util

import net.liftweb.common._
import net.liftweb.actor._

/**
 * 
 * extends a LiftActor (which has to implement messageHandler as a var) with methods roughly resembling receiveWithin/reactWithin methods of the scala.actor 
 * 
 * temporarilyWithin(<timeout>){
 * 	 <temporary message handler body>
 * }
 * 
 * nTimesWithin(<hoMany>, <timeout>){
 * 	 <temporary message handler body>
 * }
 * 
 * onceWithin(<timeout>){
 * 	 <temporary message handler body>
 * }
 * 
 * @author Ulf Schreiber
 */

trait TimedLiftActor extends LiftActor {
  private var counter : Long = 0
  
  /**
   * inner class for the temporary handler function, used to identify messageHandlers that were set by previous, unfinished calls to temporarily
   */  
  private class TemporaryHandlerFunction(val unique : Long, val waitFor : Int, val body : PartialFunction[Any, Unit], val matchCount:Int) extends PartialFunction[Any, Unit]{
	val timeout = Timeout(unique)    
	val startedAt = java.lang.System.currentTimeMillis
	val originalHandler : PartialFunction[Any, Unit] = messageHandler match{
	  // unwrap old TimeOutHandler if old will end before new to avoid unneccessary chaining of TimeoutHandlers
	  case old : TemporaryHandlerFunction if (old.startedAt+old.waitFor < startedAt+waitFor) => old.originalHandler  
	  case _ => messageHandler
	}
	val timeoutReceiver : PartialFunction[Any, Unit] = {
	  case timeout => messageHandler = originalHandler
	}  
	val countingBody = if(matchCount>0){
	  new PartialFunction[Any, Unit]{
	    var toDo = matchCount
	    override def isDefinedAt(x : Any) = body.isDefinedAt(x)
	    override def apply(x : Any) = {
	      inner.apply(x)
	      toDo -= 1
	      if(toDo<1) messageHandler = originalHandler
	    }
	  }
	}else{
	  body
	}
	val inner : PartialFunction[Any, Unit] = timeoutReceiver orElse body
	override def isDefinedAt(x : Any) = inner.isDefinedAt(x)
	override def apply(x : Any) = inner.apply(x)
	
	timer ! Reminder(TimedLiftActor.this, timeout, waitFor)
  }  
  /**
   * replace the messageHandler for at least forMillis milliseconds
   */
  final def temporarilyWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    counter += 1 // counter is used so that 
    messageHandler = new TemporaryHandlerFunction(counter, forMillis, body, 0) 
  }
  /**
   * replace the messageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched <code>times</code> times
   */
  final def nTimesWithin(times : Int, forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    counter += 1 // counter is used so that 
    messageHandler = new TemporaryHandlerFunction(counter, forMillis, body, times) 
  }
  /**
   * replace the messageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched 
   */
  final def onceWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    counter += 1 // counter is used so that 
    messageHandler = new TemporaryHandlerFunction(counter, forMillis, body, 1) 
  }
  var messageHandler : PartialFunction[Any, Unit]
}


private case class Timeout(unique : Long)
private case class Reminder(to : TimedLiftActor, message : Timeout ,  millis : Int)
private case class Remove(who : TimeOnce)



/**
 * short-lived actor that, on receiving a Reminder message sends an IGNORE message to self to wait for a response (which wont happen)
 * and then sends the message in the Reminder to the receiver defined by the Reminder (the message happens to be a Timeout(unique) but who cares)
 */
private  class TimeOnce extends LiftActor{
  object IGNORE
  object WONTHAPPEN
  
	val ignoreEverything : PartialFunction[Any, Unit] = {
	  case WONTHAPPEN => // ignores IGNORE
 	}
	val self = this
	val idle : PartialFunction[Any, Unit]= {
	  case in : Reminder => {
	    self.messageHandler = self.ignoreEverything
	    self.!!(Box[Any](Some(IGNORE)), in.millis)
	    in.to ! in.message
	    self.messageHandler = self.idle
	    timer ! Remove(self)
	  }  
	  case IGNORE => // accept any pending ignores and ignore them
	}
	var messageHandler = idle 
}

/**
 * our central manager for instances of TimeOnce
 */
private object timer extends LiftActor{
  var inflight : List[TimeOnce] = Nil
  val self = this
  def messageHandler = {
    case in : Reminder =>{
      val timer = new TimeOnce
      inflight ::=timer
      timer ! in
    }
    case Remove(timer) => inflight -= timer
  } 
}
