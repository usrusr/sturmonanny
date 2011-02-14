package de.immaterialien.sturmonanny.util

object Daemon {
  case object interrupt
  def daemon(body: => Unit): Then = {
    new Then(body, this.getClass.getSimpleName)
  }  
  def named(name:String)(body: => Unit): Then = {
    new Then(body, name)
  }
//  def daemon(body: => Unit): Then = {
//    new Then(body)
//  }
	
	  class Then(body: => Unit, name:String) extends Logging {

    def then(fin: => Unit): Thread = {
      val ret: Thread = new Thread(name) {
        override def run() {
          try {
            while (!Thread.currentThread.isInterrupted) {
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