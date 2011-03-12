package de.immaterialien.sturmonanny.util

object Daemon {
  case object interrupt
  def daemon(body: => Unit): Then = {
    new Then(body, this.getClass.getSimpleName, true)
  }  
  def named(name:String)(body: => Unit): Then = {
    new Then(body, name, true)
  }
  def once(name:String)(body: => Unit): Then = {
    new Then(body, name, false)
  }
  def once()(body: => Unit): Then = {
    new Then(body, this.getClass.getSimpleName, false)
  }  
//  def daemon(body: => Unit): Then = {
//    new Then(body)
//  }
	
	  class Then(body: => Unit, name:String, repeat:Boolean) extends Logging {

    def then(fin: => Unit): Thread = {
      val ret: Thread = new Thread(name) {
        override def run() {
          try {
          	if( ! repeat) body
          	else while (!Thread.currentThread.isInterrupted) {
              body
            }
          } catch {
            case e: Throwable => debug("Exception in daemon thread: " + e.getClass.getSimpleName + "\n" + e.getMessage + "\n" + e.getStackTraceString)
          }
          fin
        }
      }

      ret.start
      ret
    }
  }
	
}