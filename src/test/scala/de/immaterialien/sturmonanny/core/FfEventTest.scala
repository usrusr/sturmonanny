package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny._

import fbdjhosting.FbdjAdapter 
import persistence.BalanceWrapper
import scala.util.parsing.combinator._

object FfEventTest {
	def main(args: Array[String])={
		net.liftweb.actor.LAScheduler.threadPoolSize = 1
		net.liftweb.actor.LAScheduler.maxThreadPoolSize = 1
		net.liftweb.actor.LAScheduler.onSameThread = true
		//play("src/test/resources/pilotlogs/Pilot.EJGr.Ost_Irmin.log")
		//play("src/test/resources/pilotlogs/Pilot.EJGr.Ost_Yogy.log")
		//play("src/test/resources/pilotlogs/Pilot.EJGr.Ost_Yogy.log")
		play("src/test/resources/pilotlogs/refly_counted_as_lost_plane.playback")
	}
	
	
	def play(fname:String) = {
		val ff = new FfEventTest
		val offset = math.max(fname.lastIndexOf("/"), fname.lastIndexOf("\\")) + 7
		val parser = new ff.PilotLogParser(fname.drop(offset).dropRight(4))
		
		val reader = new java.io.FileReader(fname)
		parser.parseAll(parser.file, reader)
		reader.close
		println("played")
	}
}

class FfEventTest {
	class AdvancableTimeSource extends util.TimeSource {
		var time = 0L
		def set(newTime:Long)=time=newTime
		def advance(diff:Long)=time += diff
		
		def currentTimeMillis = time
	}
	
	val server = new ReplayServerImpl()
	
	class ReplayServerImpl(val initConf : String) extends Server{ Server =>
		def this() = this("default.conf") 
		private var members : List[UpdatingMember] = Nil 
		//this : Server =>
	 
		private var internalconf = new Configuration(initConf, this)  
		override def conf = internalconf  
		override def conf_= (newConf : Configuration) {  
			internalconf = newConf
			members foreach (_ updateConfiguration)
		}   
		override def shutdown = ()
		override val balance = new BalanceWrapper with Member 
		override val rules = new Rules with Member   
		override val pilots = new Pilots with Member   
		override val planes = new Planes with Member      
		override val market = new MarketActor(conf.market.implementation.apply, conf.market.configuration.apply) with Member
		override val fbdj = new FbdjAdapter with Member {
			override def updateConfiguration = ()
		}
		override val dispatcher = new LocalizedDispatcher with Member
		override val eventlog = new EventLogDispatcher with Member
		
		override val time = new AdvancableTimeSource
	
		override val multi = new Multiplexer ("", 0, 0) with Member {
			override def newServerThread = null
			override def newIl2Waiter = null
		}
	debug("init server class")	 
		init()
	}


	class PilotLogParser(pilot:String) extends RegexParsers { import de.immaterialien.sturmonanny
		override def skipWhitespace = false
		lazy val file = rep(millisLine)
		lazy val millisLine = """\d{13}:""".r ~ (source | event) <~ lf ^^ { case millis ~ ev => 
			val ms = millis.dropRight(1).toLong
			if(ev.isInstanceOf[EventSource.Source] || ev.isInstanceOf[Is.Event]){
				server.time.set(ms)
				server.pilots.forElement(pilot)(_ ! ev)
				
				println("sending "+ev)
			}else{
				println("ignoring "+ev)
			}
		} | (nlf ~ lf) ^^ {case line ~ _ => 
			println ("failed '"+line+"'")
			
		}
		lazy val source : Parser[EventSource.Source] = (
					("Console(" ~> event <~ ")") ^^ { ev:Is.Event => EventSource.Console(ev) }
				| ("Logfile(" ~> event <~ ")") ^^ { ev:Is.Event => EventSource.Logfile(ev) }
		)
		lazy val event : Parser[Is.Event] = (
			("InPlaneForSide(" ~> noncomma ~ side <~ ")") ^^ { case plane ~ side => Is.InPlaneForSide(plane, side) }
			| "Joining" ^^^ Is.Joining
			| "Selecting" ^^^ Is.Selecting
			| "Leaving" ^^^ Is.Leaving
			| "MissionBegin" ^^^ Is.MissionBegin
			| "MissionEnd" ^^^ Is.MissionEnd
			| "Crashing" ^^^ Is.Crashing
			| "Dying" ^^^ Is.Dying
			| "Killed" ^^^ Is.Killed			
			| "KIA" ^^^ Is.KIA			
			| "InFlight" ^^^ Is.InFlight
			| "LandedAtAirfield" ^^^ Is.LandedAtAirfield
			| "HitTheSilk" ^^^ Is.HitTheSilk
			| "Ejecting" ^^^ Is.Ejecting			
			| ("MissionChanging"~"(" ~> "[^\\)]+".r <~ ")") ^^ {what:String=> Is.MissionChanging(new java.io.File(what))}
			| ("TakingSeat(" ~> "[^\\)]+".r <~")") ^^ { plane => Is.TakingSeat(plane) } 
			| ("Loading(" ~> noncomma ~ noncomma ~ double <~")") ^^ { case plane ~load~ fuel => Is.Loading(plane, load, fuel) } 
			| ("UserState("~>event<~(")")) ^^ {ev=>EventSource.UserState(ev)}
			| ("Chatting("~>"""[^\r\n]*(?=\)\))""".r <~")") ^^^ Is.Ignored
//1294170608757:Console(InPlaneForSide(P-51B-NA,Red))
//			| Loading(Fw-190F-8,1sc5004sc50,0.0)
		)
		lazy val side : Parser[Armies.Armies] = (
				("None" ^^^ Armies.None) 
			| ("Blue" ^^^ Armies.Blue)
			| ("Red" ^^^ Armies.Red)
		)
		lazy val double = """\d+(\.\d+)""".r ^^ (_ toDouble)
		lazy val noncomma = "[^,\\)]*".r <~","
		lazy val lf = """[\r\n]+""".r 
		lazy val nlf = """[^\r\n]+""".r 
		
	}

}