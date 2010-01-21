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
 * @author Ulf Schreiber
 */

trait TimedLiftActor extends LiftActor { 
//with Logging {
//loglevel=LiftLogLevels.Trace

  /**
   * replace the temporaryMessageHandler for at least forMillis milliseconds
   */
  final def reactWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, -1) 
  }
  /**
   * replace the temporaryMessageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched <code>times</code> times
   */
  final def reactNTimesWithin(times : Int, forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, times) 
  }
  /**
   * replace the temporaryMessageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched 
   */
  final def reactOnceWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, 1) 
  }
  /**
   * return to the last temporaryMessageHandler, even before timeout 
   */
  final def reactNormally() = {
	temporaryMessageHandler = temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction => handler.originalHandler
	  case _ => temporaryMessageHandler
	}
  }
  /**
   * add more time to current nonstandard handler 
   */
  final def extendTime(additionalMillis : Long ) = {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction => handler.until += additionalMillis
	  case _ => temporaryMessageHandler
	}
  }
  /**
   * add more matches to current nonstandard handler (if it is counting) 
   */
  final def extendCount(additionalCount : Int) = {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction if(handler.matchCount > 0) => handler.toDo += additionalCount
	  case _ => temporaryMessageHandler
	}
  }
  /**
   * set new timeout
   */
  final def extendTimeFromNow(millis : Long ) = {
	temporaryMessageHandler match {
	  case handler : TemporaryHandlerFunction => handler.until = java.lang.System.currentTimeMillis + millis
	  case _ => temporaryMessageHandler
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
  
  private var temporaryMessageHandler : PartialFunction[Any, Unit] = defaultMessageHandler
  override final def messageHandler : PartialFunction[Any, Unit]  = temporaryMessageHandler 

  final def mes = 0;
  
  def defaultMessageHandler : PartialFunction[Any, Unit] 
  
  /**
   * inner class for the temporary handler function, used to identify messageHandlers that were set by previous, unfinished calls to temporarily
   */  
  private case class TemporaryHandlerFunction(val waitFor : Int, val body : PartialFunction[Any, Unit], val matchCount:Int) extends PartialFunction[Any, Unit]{
	var until = java.lang.System.currentTimeMillis + waitFor
 
	val originalHandler : PartialFunction[Any, Unit] = temporaryMessageHandler match{
	  // unwrap old TimeOutHandler if old will end before new to avoid unneccessary chaining of TimeoutHandlers
	  case old : TemporaryHandlerFunction if (old.until < until) => {
	    old.originalHandler
      }
	  case _ => {
	    temporaryMessageHandler
     }
	}

    var toDo = matchCount
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
 
	def ranOut() = (matchCount>0 && toDo<1) || (java.lang.System.currentTimeMillis>until)
	
 
	override def isDefinedAt(x : Any) = {
	  if( ! ranOut()) inner.isDefinedAt(x)
	  else {
		  temporaryMessageHandler = originalHandler
		  temporaryMessageHandler.isDefinedAt(x)
	  }
   	}
	override def apply(x : Any) = {
	  if( ! ranOut()) inner.apply(x)
	  else {
		  temporaryMessageHandler = originalHandler
		  temporaryMessageHandler.apply(x)
	  }
	}
  }  
}
