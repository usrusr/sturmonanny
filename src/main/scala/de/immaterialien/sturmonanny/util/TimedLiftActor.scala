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
 * 
 * reactNormally()
 * 
 * and a number of extend... methods to manipulate the timeout/countout of the current handler
 * 
 * implement defaultMessageHandler instead of messageHandler
 * 
 * beware of confusion between handler functions with "catchall" and handler functions without, the catchall-versions might steal your messages! 
 *
 *
 * all three
 * react...{
 * 	 <temporary message handler body>
 * }
 * can be followed by 
 * andThen{
 * 	 <"and then" body>
 * }
 * which will be called on reactNormally as well as on timeout/countout 
 * 
 * @author Ulf Schreiber
 */

trait TimedLiftActor extends LiftActor with TimeHolder{ 
//with Logging {
//loglevel=LiftLogLevels.Trace
	def time : TimeSource  
  /**
   * replace the temporaryMessageHandler for at least forMillis milliseconds
   */
  final def reactWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) = {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, -1, "within "+forMillis)
    new AndThen(temporaryMessageHandler)
  }
  /**
   * replace the temporaryMessageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched <code>times</code> times
   */
  final def reactNTimesWithin(times : Int, forMillis : Int)(body : PartialFunction[Any, Unit] ) {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, times, times + " times within "+forMillis)
    new AndThen(temporaryMessageHandler)
  }
  /**
   * replace the temporaryMessageHandler for at least <code>forMillis</code> milliseconds or until the temporary handler has matched 
   */
  final def reactOnceWithin(forMillis : Int)(body : PartialFunction[Any, Unit] ) {
    temporaryMessageHandler = new TemporaryHandlerFunction(forMillis, body, 1, "once within "+forMillis)
    new AndThen(temporaryMessageHandler)
  }
  /**
   * return to the last temporaryMessageHandler, even before timeout 
   */
  final def reactNormally() {
	  temporaryMessageHandler.revert
  }

  
  /**
   * add more time to current nonstandard handler 
   */
  final def extendTime(additionalMillis : Long ) {
	    temporaryMessageHandler.until += additionalMillis
	    temporaryMessageHandler.requestPing
  }
  /**
   * add more matches to current nonstandard handler (if it is counting) 
   */
  final def extendCount(additionalCount : Int) {
	  if(temporaryMessageHandler.matchCount > 0) temporaryMessageHandler.toDo += additionalCount
  }
  /**
   * set new timeout
   */
  final def extendTimeFromNow(millis : Long ) {
	    temporaryMessageHandler.until = time.currentTimeMillis + millis
	    temporaryMessageHandler.requestPing
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

  private var internalTemporaryMessageHandler : TemporaryHandlerFunction = null
  private def temporaryMessageHandler : TemporaryHandlerFunction = internalTemporaryMessageHandler
  private def temporaryMessageHandler_=(replacement : TemporaryHandlerFunction ) {
    internalTemporaryMessageHandler = replacement
    this ! Reevaluate // force reevaluation of mailbox
    
  }
//val debugFile = new java.io.FileWriter("actor."+this.getClass.getSimpleName+".log")

  final def messageHandlerDef() = messageHandlerFunction
  override final val messageHandler : PartialFunction[Any, Unit]  = new PartialFunction[Any, Unit]{
    def isDefinedAt(x:Any):Boolean={
//debugFile.write("isdef?"+x+"\n <- "+messageHandlerFunction+"\n")
//debugFile.flush
    
//      messageHandlerFunction.isDefinedAt(x)
val ret = messageHandlerDef().isDefinedAt(x)  
//debugFile.write("isdef?"+x+"\n <- "+messageHandlerFunction + " || "+messageHandlerDef()+" -> "+ret+" \n")
//debugFile.flush
ret    
    }
  
    def apply(x:Any){
//debugFile.write("apply:"+x+"\n <- "+messageHandlerFunction+"\n")
//debugFile.flush
//      messageHandlerFunction.apply(x)
   
val ret = messageHandlerDef().apply(x)
//debugFile.write("apply:"+x+"\n <- "+messageHandlerFunction + " || "+messageHandlerDef()+" -> "+ret+" \n")
//debugFile.flush
ret      
    }
  }
//  final def messageHandler = messageHandlerFunction
  
  final def messageHandlerFunction : PartialFunction[Any, Unit]  = {
    if(internalTemporaryMessageHandler == null) internalTemporaryMessageHandler = new TemporaryHandlerFunction(-1, defaultMessageHandler,-1, "base handler")

    temporaryMessageHandler
  } 

  final def mes = 0;
  private case object Reevaluate
  private case class TimeOut(val until : Long){
if(until==0) println("0 until from "+new Exception().getStackTraceString)    
  }
  
  class AndThen(from:TemporaryHandlerFunction) {
	  	def andThen(body : =>Unit):Unit={
	  		from.andThen = body _
	    }
  }
  
  /**
   * inner class for the temporary handler function, used to identify messageHandlers that were set by previous, unfinished calls to temporarily
   */  
  protected case class TemporaryHandlerFunction(val waitFor : Int, val body : PartialFunction[Any, Unit], val matchCount:Int, val name:String) extends PartialFunction[Any, Unit] with Logging{
    // interface for calling andThen
	  var andThen : Function0[Unit] = _
	  
		var until = time.currentTimeMillis + waitFor
	    var toDo = matchCount
	 	def ranOut() = (matchCount>0 && toDo<1) || (time.currentTimeMillis>until)
	 	def requestPing() {
	//debug(" requesting ping in  "+new Exception().getStackTraceString)	  
	 	  LAPinger.schedule(TimedLiftActor.this, TimeOut(until), until - time.currentTimeMillis )
		}
		if(internalTemporaryMessageHandler!=null) requestPing
		
		val originalHandler : TemporaryHandlerFunction = 
    if (temporaryMessageHandler==null) this
    else temporaryMessageHandler match{
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
		      if(toDo<1) revert
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
		/**
     *  pops the handler off the stack and calls andThen 
     */
		def revert {
		  if(originalHandler!=null && originalHandler!=this) temporaryMessageHandler = originalHandler
		  andThen
		}
		override def isDefinedAt(x : Any) = {
	//println("isDefinedAt called "+ x )	
		  if(x == Reevaluate){
		    true
		  }else if( ! ranOut) {
		    timeoutOrDefinedAt(x, inner)
		  } else {
			  if(originalHandler!=null && originalHandler!=this){
				  revert
				  timeoutOrDefinedAt(x, originalHandler)// or body?
			  }else{
				  timeoutOrDefinedAt(x, body)
			  }
		  } 
	   	}
	 	def timeoutOrApply(x:Any, partial : PartialFunction[Any, Unit])= x match {
	      case TimeOut(to) if (to==until) => {
	        revert
	      }
	      case TimeOut(_) => ()
	      case Reevaluate => ()
	      case _ => partial apply x
		}
	 
		override def apply(x : Any) = {
	//debug("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< apply called "+ x +" in until "+until)	  
		  if( ! ranOut) {
		    timeoutOrApply(x, inner)
	//	    inner.apply(x)
		  }else {
		    timeoutOrApply(x, inner)
		  }
		}
		override def toString = {
		  name + " (until "+until+" or "+toDo+") "+(if(originalHandler!=this)" <- " + originalHandler)
		}
  }  
}
