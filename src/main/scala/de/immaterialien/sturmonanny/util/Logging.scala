package de.immaterialien.sturmonanny.util

import net.liftweb.util._

trait Logging {
 // this : Lg => Logging 
//trait Logging[Lg] {
//  this : Lg with Logging[Lg] =>
  private val logger = LogBoot.loggerByName(this.getClass.getSimpleName) 
  def isTraceEnabled: Boolean = logger.isTraceEnabled
  def trace(msg: => AnyRef): Unit = logger.trace(msg)
  def trace(msg: => AnyRef, t: => Throwable): Unit = logger.trace(msg,t)

  def assertLog(assertion: Boolean, msg: => String): Unit = logger.assertLog(assertion, msg)

  
  def isDebugEnabled: Boolean = logger.isDebugEnabled
  def debug(msg: => AnyRef): Unit = logger.debug(msg)
  def debug(msg: => AnyRef, t: => Throwable): Unit = logger.debug(msg,t)
  
  def isErrorEnabled: Boolean = logger.isErrorEnabled
  def error(msg: => AnyRef): Unit = logger.error(msg)
  def error(msg: => AnyRef, t: => Throwable): Unit = logger.error(msg,t)



  def fatal(msg: AnyRef): Unit = logger.fatal(msg)
  def fatal(msg: AnyRef, t: Throwable): Unit = logger.fatal(msg, t)

  def loglevel: LiftLogLevels.Value = logger.level
  def loglevel_=(level: LiftLogLevels.Value): Unit = logger.level = level
  private def name: String = logger.name
//  def parent = logger.getParent

  def isInfoEnabled: Boolean = logger.isInfoEnabled
  def info(msg: => AnyRef): Unit = logger.info(msg)
  def info(msg: => AnyRef, t: => Throwable): Unit = logger.info(msg,t)

  def isEnabledFor(level: LiftLogLevels.Value): Boolean = logger.isEnabledFor(level)

  def isWarnEnabled: Boolean = logger.isWarnEnabled
  def warn(msg: => AnyRef): Unit = logger.warn(msg)
  def warn(msg: => AnyRef, t: => Throwable): Unit = logger.warn(msg,t)
  
  loglevel = LiftLogLevels.Trace
}
