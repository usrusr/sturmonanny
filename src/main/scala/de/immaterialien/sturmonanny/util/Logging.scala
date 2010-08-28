package de.immaterialien.sturmonanny.util

import net.liftweb.util._
import net.lag.logging.Logger

trait Logging { 
 // this : Lg => Logging     
//trait Logging[Lg] {
//  this : Lg with Logging[Lg] => 
  //private val logger = LogBoot.loggerByName(this.getClass.getSimpleName)
  protected def initLog = Logger.get(this.getClass)
  val logger = initLog
  def isTraceEnabled: Boolean = logger.isLoggable(Logger.TRACE)
  def trace(msg: => AnyRef): Unit = logger.trace(msg.toString)
  def trace(msg: => AnyRef, t: => Throwable): Unit = logger.trace(t, msg.toString+" "+t.getClass.getSimpleName)
   
  def isDebugEnabled: Boolean = logger.isLoggable(Logger.DEBUG)
  def debug(msg: => AnyRef): Unit = logger.debug(msg.toString)
  def debug(msg: => AnyRef, t: => Throwable): Unit = logger.debug(t,msg.toString+" "+t.getClass.getSimpleName)
  
  def isErrorEnabled: Boolean = logger.isLoggable(Logger.ERROR)
  def error(msg: => AnyRef): Unit = logger.error(msg.toString)
  def error(msg: => AnyRef, t: => Throwable): Unit = logger.error(t,msg.toString +" "+t.getClass.getSimpleName)

 

  def fatal(msg: AnyRef): Unit = logger.fatal(msg.toString)
  def fatal(msg: AnyRef, t: Throwable): Unit = logger.fatal(t,msg.toString+" "+t.getClass.getSimpleName)


  private def name: String = logger.name

  def isInfoEnabled: Boolean = logger.isLoggable(Logger.INFO)
  def info(msg: => AnyRef): Unit = logger.info(msg.toString)
  def info(msg: => AnyRef, t: => Throwable): Unit = logger.info(t,msg.toString+" "+t.getClass.getSimpleName) 

  def isWarnEnabled: Boolean = logger.isLoggable(Logger.WARNING)
  def warn(msg: => AnyRef): Unit = logger.warning(msg.toString)
  def warn(msg: => AnyRef, t: => Throwable): Unit = logger.warning(t,msg.toString+" "+t.getClass.getSimpleName)

  def isEnabledFor(level: net.lag.logging.Level): Boolean = logger.isLoggable(level)
}
trait Log {    
   
  object logging extends Logging {
    override def initLog = Logger.get(Log.this.getClass)
  }
  val log = logging.logger
}
