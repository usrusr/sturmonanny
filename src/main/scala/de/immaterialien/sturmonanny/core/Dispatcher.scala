package de.immaterialien.sturmonanny.core

//import net.liftweb.actor.LiftActor
import de.immaterialien.sturmonanny.util._

/**
 * a place for message parsing to grow and evolve without being disturbed by the necessities of console connections, game logic or configuration
 */
class Dispatcher extends TimedLiftActor with NonUpdatingMember{  
	 def fromServer(lines : Seq[String]) {
	   
	 }
	 override def defaultMessageHandler = {
	   case Dispatcher.pilotsHeader => reactWithin(50){
		   case Dispatcher.pilotFlying(id, name, ping, score, army, plane) => {
		     server.pilots ! forward(name, flies(plane, Armies.forName(army)))
		     extendTimeFromNow(50)
		   }
	   }
	   case _ => 
	 }
}

object Dispatcher {
  val pilotsHeader = """ N       Name           Ping    Score   Army        Aircraft\s*""".r
  val pilotFlying = """(\d+)\s+(\S+)\s+(\d+)\s+(-?\d+)\s+\(\d\)(\S+)\s+(\S*)\s*""".r
  val separator = """-------------------------------------------------------.\s*""".r;
}
