package de.immaterialien.sturmonanny.core

//import net.liftweb.actor.LiftActor
import de.immaterialien.sturmonanny.util._

/**
 * a place for message parsing to grow and evolve without being disturbed by the necessities of console connections, game logic or configuration
 */
class Dispatcher extends TimedLiftActor with NonUpdatingMember with Logging{   
	 def fromServer(lines : Seq[String]) {
	   
	 }
  
val debugWriter = new java.io.FileWriter("dispatcher.out.txt")   
  
	 override def defaultMessageHandler = new PartialFunction[Any, Unit](){
	   override def isDefinedAt(x : Any) = {
//println("isDefinedAt called "+ x )	  
		  internal isDefinedAt(x)
	  } 
   	
	override def apply(x : Any) = {
//debug("dispatching... "+ x )	   
		  internal apply(x)
	  }
	
	 }
  
	 val internal :  PartialFunction[Any, Unit] = {
	 //override def defaultMessageHandler = {
	   case Dispatcher.pilotsHeader(_) => {
debug("dispatch pilots header ")	     
	     reactWithin(50){
	       
		   case Dispatcher.pilotFlying(id, name, ping, score, army, plane) => {
debug("dispatch pilot flying "+id+","+name+","+ping+","+score+","+army+","+plane)	     
		     server.pilots ! Domain.forward(name, flies(plane, Armies.forName(army)))
		     extendTimeFromNow(500) 
		   }
		   case x => {
debug(" --- temp did not understand '"+x+"'") 		     
		     reactNormally
		     this ! x
		   }
	     }
	   }
       case Dispatcher.playerJoin(who) => {
debug("dispatch player join '"+who+"'")
			server.pilots ! new server.pilots.Pilot(who)
       }
       case Dispatcher.missionIsPlaying(what) => {
debug("new mission '"+what+"'")
			server.market.cycle(what)
       }
       case Dispatcher.chat(who, what) => {
debug(who+" chats '"+what+"'")
         	server.pilots ! Domain.forward(who, chats(what))
       }
	   
//	   case Dispatcher.separator => reactOnceWithin(50){
//	     
//	     case Dispatcher.separator => reactNormally
//	   }
	   case x => {
  debug(" --- did not understand '"+x+"'")
if(isDebugEnabled) {
  debugWriter.append("'"+x+"'\n")
  debugWriter flush
}

	   }
	 }
}

object Dispatcher {
  val missionIsPlaying = """Mission\: (.+) is Playing\\n""".r
  val playerJoin = """socket channel '\d+', ip (\S+)\:(\d+), (.+), is complete created\\n""".r
  val pilotsHeader = ("""(\\"""+"""u0020N       Name           Ping    Score   Army        Aircraft)\\n""").r
  val pilotFlying = (
		  """\\""" + """u0020"""+ 		// intro
		  """(\d+)\s+"""+ 				// ID number and some blanks
		  """(\S(?:.{0,14}\S)?)\s+"""+	// name (tolerating blanks and having a maximum length) and some blanks
		  """(\d+)\s+"""+				// ping and some blanks
		  """(\d+)\s+"""+				// score and some blanks
		  """\(\d+\)(\S+)\s+"""+		// army with prefixed number (ignored) and some blanks
		  """.{0,11}\s"""+				// ignore plane registration (with a maximum length, and at least one blank)
		  """((?:\S.*)?)\\n"""			// plane before end (does it need to tolerate blanks?)
  	).r
  val chat = """Chat: (.+): \\t(.*)\\n""".r
  val hasLeftTheGame = """Chat: --- (.+) has left the game\.\\n'""".r
  val joinsTheGame = """Chat: --- (.+) joins the game\.\\n'""".r
  
  val seqSeparator = """-------------------------------------------------------\\n""".r;
  val seqName = """Name\: \\t(.*)\\n""".r
                              
}
