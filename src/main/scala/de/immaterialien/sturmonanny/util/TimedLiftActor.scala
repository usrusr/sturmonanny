package de.immaterialien.sturmonanny.util

import net.liftweb.common._
import net.liftweb.actor._


trait TimedLiftActor extends LiftActor {
  private var counter : Long = 0
  final def temporarily(forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    counter += 1
    class TimeoutHandler(val unique : Long, val waitFor : Long, val body : PartialFunction[Any, Unit]) extends PartialFunction[Any, Unit]{
    	val timeout = Timeout(unique)    
    	val startedAt = java.lang.System.currentTimeMillis
    	val temp : PartialFunction[Any, Unit] = messageHandler match{
    	  // unwrap old TimeOutHandler if old will end before new to avoid unneccessary chaining of TimeoutHandlers
    	  case old : TimeoutHandler if (old.startedAt+old.waitFor < startedAt+waitFor) => old.temp  
    	  case _ => messageHandler
    	}
    	val inner : PartialFunction[Any, Unit] = {
	      case timeout => messageHandler = temp
	      body
    	}
    	override def isDefinedAt(x : Any) = inner.isDefinedAt(x)
    	override def apply(x : Any) = inner.apply(x)

        timer ! Reminder(TimedLiftActor.this, timeout, forMillis)
    }
    
    counter += 1 // counter is used so that 
    messageHandler = new TimeoutHandler(counter, forMillis, body) 
  }
  var messageHandler : PartialFunction[Any, Unit]
}
private case class Timeout(unique : Long)
private case class Reminder(to : TimedLiftActor, message : Timeout ,  millis : Int)
private case class Remove(who : TimeOnce)

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
