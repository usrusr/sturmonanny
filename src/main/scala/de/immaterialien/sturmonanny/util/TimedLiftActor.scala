package de.immaterialien.sturmonanny.util

import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._

/**
 * 
 * extends a LiftActor (which has to implement temporaryMessageHandler as a var) with methods roughly resembling receiveWithin/reactWithin methods of the scala.actor 
 * 
 * reactWithin(<timeout>){
 * 	 <temporary message handler body> 
 * }
 * 
 * reactNTimesWithin(<hoMany>, <timeout>){
 * 	 <temporary message handler body>
 * }
 * 
 * reactOnceWithin(<timeout>){
 * 	 <temporary message handler body>
 * }
 * 
 * reactNormally()
 * 
 * and a number of extend... methods to manipulate the timeout/countout of the current handler
 * 
 * implement defaultMessageHandler instead of messageHandler
 * 
 * beware of confusion between handler functions with "catchall" and handler functions without, the catchall-versions might steal your messages! 
 * 
 * @author Ulf Schreiber
 */

trait TimedLiftActor extends LiftActor { 
//with Logging {
//loglevel=LiftLogLevels.Trace

  /**
   * replace the temporaryMessageHandler for at least forMillis milliseconds
   */
  final def reactWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, -1) 
  }
  /**
   * replace the temporaryMessageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched <code>times</code> times
   */
  final def reactNTimesWithin(times : Int, forMillis : Int)(body : PartialFunction[Any, Unit] ) {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, times) 
  }
  /**
   * replace the temporaryMessageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched 
   */
  final def reactOnceWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, 1) 
  }
  /**
   * return to the last temporaryMessageHandler, even before timeout 
   */
  final def reactNormally() {
	temporaryMessageHandler = temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction => handler.originalHandler
	  case _ => temporaryMessageHandler
	}
  }

  
  /**
   * add more time to current nonstandard handler 
   */
  final def extendTime(additionalMillis : Long ) {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction => {
	    handler.until += additionalMillis
	    handler.requestPing
     }
	  case _ => 
	}
  }
  /**
   * add more matches to current nonstandard handler (if it is counting) 
   */
  final def extendCount(additionalCount : Int) {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction if(handler.matchCount > 0) => handler.toDo += additionalCount
	  case _ => 
	}
  }
  /**
   * set new timeout
   */
  final def extendTimeFromNow(millis : Long ) {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction => {
	    handler.until = java.lang.System.currentTimeMillis + millis
	    handler.requestPing
      }
	  case _ => 
	}
  }  
  /**
   * set new maximum matches, from now (unless the handler started unlimited) 
   */
  final def extendCountFromNow(newCount : Int) = {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction if(handler.matchCount > 0)  => handler.toDo = newCount
	  case _ => temporaryMessageHandler
	}
  }
  def defaultMessageHandler : PartialFunction[Any, Unit] 

  //private var temporaryMessageHandler : PartialFunction[Any, Unit] = defaultMessageHandler

  private var internalTemporaryMessageHandler : PartialFunction[Any, Unit] = defaultMessageHandler
  def temporaryMessageHandler  = internalTemporaryMessageHandler
  def temporaryMessageHandler_=(replacement : PartialFunction[Any, Unit] ) {
    internalTemporaryMessageHandler = replacement
    this ! TimeOut(0) // force reevaluation of mailbox
  }
  
  override final def messageHandler : PartialFunction[Any, Unit]  = {
    if(temporaryMessageHandler == null) temporaryMessageHandler = defaultMessageHandler

    temporaryMessageHandler
  } 

  final def mes = 0;
  
  private case class TimeOut(val until : Long)
  /**
   * inner class for the temporary handler function, used to identify messageHandlers that were set by previous, unfinished calls to temporarily
   */  
  private case class TemporaryHandlerFunction(val waitFor : Int, val body : PartialFunction[Any, Unit], val matchCount:Int) extends PartialFunction[Any, Unit] with Logging{
	var until = java.lang.System.currentTimeMillis + waitFor
    var toDo = matchCount
 	def ranOut() = (matchCount>0 && toDo<1) || (java.lang.System.currentTimeMillis>until)
 	def requestPing() {
	  LAPinger.schedule(TimedLiftActor.this, TimeOut(until), until - java.lang.System.currentTimeMillis )
	}
	requestPing
	val originalHandler : PartialFunction[Any, Unit] = temporaryMessageHandler match{
	  // unwrap old TimeOutHandler if old will end before new to avoid unneccessary chaining of TimeoutHandlers
	  case old : TemporaryHandlerFunction if (old.until < until) => {
	    old.originalHandler
      }
	  case _ => {
	    temporaryMessageHandler
     }
	}

	val countingBody = if(matchCount>0){
	  new PartialFunction[Any, Unit]{
	    override def isDefinedAt(x : Any) = body.isDefinedAt(x) 
	    override def apply(x : Any) = {
	      inner.apply(x)
	      toDo -= 1
	      if(toDo<1) temporaryMessageHandler = originalHandler
	    }
	  }
	}else{
	  body 
	}
	var inner : PartialFunction[Any, Unit] = countingBody
 
	def timeoutOrDefinedAt(x:Any, partial : PartialFunction[Any, Unit])= x match {
      case TimeOut(_)=> true
      case _ => partial isDefinedAt x
	}
 
	override def isDefinedAt(x : Any) = {
//println("isDefinedAt called "+ x )	  
	  if( ! ranOut) {
	    timeoutOrDefinedAt(x, inner)
	  } else {
		  temporaryMessageHandler = originalHandler
		  timeoutOrDefinedAt(x, temporaryMessageHandler)
	  } 
   	}
 	def timeoutOrApply(x:Any, partial : PartialFunction[Any, Unit])= x match {
      case TimeOut(to) if (to==until) => {
        temporaryMessageHandler = originalHandler
      }
      case TimeOut(_) => ()
      case _ => partial apply x
	}
 
	override def apply(x : Any) = {
//debug("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< apply called "+ x )	  
	  if( ! ranOut) {
	    timeoutOrApply(x, inner)
//	    inner.apply(x)
	  }else {
	    timeoutOrApply(x, inner)
	  }
	}
  }  
}
