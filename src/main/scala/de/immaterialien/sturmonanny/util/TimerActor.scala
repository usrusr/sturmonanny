package de.immaterialien.sturmonanny.util

import scala.collection._
import net.liftweb.actor.LiftActor


class TimerActor(var pause : Long) extends Thread with LiftActor  { 
  def this(p : Int) = this(p.toLong)
  	private case class SetInterval(val millis : Long)
    private case class add(val who : LiftActor)
    private case class remove(val who : LiftActor)
  
    case class passed(val millis : Long)

    def setInterval( millis : Long) {
      this ! SetInterval(millis)
    }
    def setInterval( millis : Int) {
      this ! SetInterval(millis)
    }

    def unsubscribe( who : LiftActor) {
      this ! add(who)
    }
    def subscribe( who : LiftActor) {
      this ! remove(who) 
    }
    override def run = while(! this.isInterrupted ){
       Thread.sleep(pause)
	   this ! passed(pause)
    }
//    var pause : Int = 60*1000 
    val multiplexers :mutable.Set[LiftActor]= (new jcl.WeakHashMap(new java.util.WeakHashMap)).keySet
    def messageHandler = {
      case SetInterval(mil) => pause=mil
      case add(who) => multiplexers += who
      case remove(who) => multiplexers -= who
      case p : passed => multiplexers map ( _ ! p) 
    }
    this setDaemon true
    this setName "sturmonanny timer  "+pause+"ms"
    this setPriority Thread.MIN_PRIORITY
    this start
}
