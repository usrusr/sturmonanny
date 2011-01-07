package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util._
import scala.util.parsing.combinator._
import net.liftweb.actor.LiftActor
import scala.io._
import scala.collection.mutable
import scala.util.matching.Regex
//import java.util.regex.Pattern



object EventLogDispatcher {

  private val PILOTNAMETIMEOUT = 60*1000*1000
}
class EventLogDispatcher extends LiftActor with UpdatingMember with RegexParsers with Logging {
	override def skipWhitespace = false  
	def regexMatch(r : String): Parser[Regex.Match] = regexMatch(r.r) 
	def regexMatch(r : Regex) : Parser[Regex.Match] = Parser { 
		in => regex(r)(in) match { // see http://www.scala-lang.org/node/1943#comment-15674
			case Success(aString, theRest) => Success(r.findFirstMatchIn(aString).get, theRest)
			case f@Failure(_,_) => f
			case e@Error(_,_) => e
		}
	}
	var maxAge = 10 
 	
 	def updateConfiguration = {
			
	} 

	
	
 	def pilotMessageSend(who:String, what: Is.Event) = server.pilots.forElement(who)(_!  EventSource.Logfile(what))
 	def allPilotMessageSend(what: Is.Event) = {
 		val msg = EventSource.Logfile(what)
 		server.pilots.forMatches("")(_!msg) 
 	}
	override def messageHandler : PartialFunction[Any, Unit] = {	  
	  case DispatchLine(x) => processLine(x)
	  case DispatchMessage(x) => processMessage(x) 
	  case _ => // ignore  
	} 
	
	/**
	 * should be exactly like parseOneLine, but not tolerating the unknown
	 */
 	def testOneLine(line:String) = {
//println("testing: '"+line+"'") 		
 		parse(lineParser, line.stripLineEnd) 
 	}
 	def parseOneLine(line:String) = {
//println("parsing: '"+line+"'") 		
 		parse(rootLineParser, line.stripLineEnd) 
 	}
	def processLine(line:String) : Unit = {
		val parseResult : ParseResult[_] = parseOneLine( line )
debug("-------------------------\nparsing line '"+line+"' \n  -> '"+parseResult+"'")  
//log.write("/===line:===\n")
//log.write(line)
//log.write("\n------->\n")
//log.write(""+parseResult)
//log.write("\n<-------\n")
//log.flush
		parseResult.getOrElse(None) match { 
		      case PilotMessage(who, Is.Ignored, _) => // ignore
		      case PilotMessage(who, Is.Unknown, _) => debug("unkown message: '"+line+"'")
		      case PilotMessage(who, what, _ ) => {
		    	  pilotMessageSend(who, what)
debug("success "+who+" -> "+what+"  from '"+line+"'")		        
		      }
		      
		    case GlobalMessage(Is.MissionChanging(mis)) => {
println("mission changing!")		    	
          server.market.cycle(mis) 
        	allPilotMessageSend(Is.MissionChanging(mis))
		    }
        case GlobalMessage(Is.MissionEnd) => {
        	allPilotMessageSend(Is.MissionEnd)
        }
        case GlobalMessage(Is.MissionBegin) => {
          allPilotMessageSend(Is.MissionBegin)
        } 
		      case None => {
debug("no parseResult: None from '"+line+"'")		        
        }
          
        case Is.Ignored =>
        case x:AnyRef => debug("got  "+x+"\n  "+x.getClass.getSimpleName+"\n   from '"+line+"'")
        case x => debug("got value "+x)
		}   
	}
 
 	def processMessage(lines:String) : Unit = {
		val parseResult  = parseAll(rootMessageParser, lines+"\n")
debug("=============================\nparsing message '"+lines+"' \n  -> '"+parseResult+"'")  
		val resList = parseResult getOrElse List()
//log.write("/===message:===\n")
//log.write(lines)
//log.write("\n------->\n")
//log.write(""+parseResult)
//log.write("\n<-------\n")
//log.flush  
		for (res <- resList) { 
		  res match { 
		      case PilotMessage(who, Is.Ignored,_) => // ignore
		      case PilotMessage(who, Is.Unknown,_) => debug("unkown message: '"+lines+"'")
		      case PilotMessage(who, what ,_) => {
		    	  pilotMessageSend(who, what)
debug("success from message: "+who+" -> "+what+"  from '"+lines+"'")		        
		       }
		  }
		}
	}
 
  	lazy val rootMessageParser : Parser[Seq[Message]]= ( 
			(lineParser <~ "\n")^^(lineResult=>List(lineResult))
	)
  
	lazy val rootLineParser = (
			lineParser 
			| (".*".r^^^ Is.Ignored)
	)
	lazy val lineParser : Parser[Message]= (
	  dateTimeParser ~> eventParser
	)
	
	lazy val maxAgeLineParser : Parser[Message]= (
	  (dateTimeParser ~ eventParser) ^^ {
	  	case date ~ msg => {
	  		val age = server.time.currentTimeMillis - date.getTimeInMillis 
	  		if( age > maxAge*1000) {
debug("skipping "+msg+" because it is too old: "+(age/1000)+"s")	  			
	  			GlobalMessage(Is.Ignored)
	  		}else msg
	  	}
	  }
	)
	
	lazy val dateTimeParser : Parser[java.util.Calendar] = (
	  "[" ~ dayParser  ~ hmsParser ~ ":" ~ hmsParser~":" ~ hmsParser ~ (" AM" ^^^ 0 | " PM" ^^^ 12) ~ "] " 
	  	^^ { case  _ ~ day ~ hour ~_~ minute ~_~ second ~ amPm ~ _  =>  
	  		import java.util.Calendar
	  	  val x:java.util.Date = day
	  	  val offs : Int = amPm
	  	  val ret = Calendar.getInstance
	  	  if(day!=null) ret setTime day
	  	  ret set(Calendar.HOUR, hour + amPm)
	  	  ret set(Calendar.MINUTE, minute)
	  	  ret set(Calendar.SECOND, second)
	  	  ret
	  }
	)

  var dayState : java.util.Date = _
  val dateformat = new java.text.SimpleDateFormat("MMM d, yyyy", java.util.Locale.ENGLISH)
 	lazy val dayParser : Parser[java.util.Date] = {
    """\w{3} \d{1,2}, \d{4} """.r ^^ 
    {string=>
      dayState = dateformat.parse(string)
      dayState
    } | ("" ^^^ dayState)
 	}

	lazy val hmsParser : Parser[Int] = {
	  """\d\d?""".r ^^ (_ toInt)
	}
	lazy val fuelParser : Parser[Int] = {
	  "fuel "~> """\d+""".r <~"%" ^^ (_ toInt)
	}
 	lazy val eventParser : Parser[Message] = (
		  "Mission BEGIN" ^^^ GlobalMessage(Is.MissionBegin)
    | "Mission END" ^^^ GlobalMessage(Is.MissionEnd)
    | ("Mission: " ~> "\\S.*\\.mis".r <~ " is Playing") ^^ (x => GlobalMessage(Is.MissionChanging{ import java.io._
//    			server.conf.server.
//    			()
//    			x+".mis" 
    			new File(conf.server.serverPath.apply+File.separator+"Missions"+File.separator+x)
    	}))
    | connectParser
    | loading
      	|	seatOccupied
        | inFlight
        | landed  
        | wasKilled  
        | ejected
        | refly
        | disconnecting
 	)
//    lazy val PilotMessageParser : Parser[PilotMessage] = {
//      ( 
//      ) ~ atLocationParser ^^ 
//      {
//        case what ~ where => PilotMessage(what.who, what.event, where) 
//      }
//
//  }
  /**
   * share pilot name knowledge with the console parser
   */
//  def pilotNameParser : Parser[String] = {
//    server.dispatcher.pilotNameParser 
//  }
  
  object pilotNameParserObj extends Parser[String] {
    def apply(in:Input)={
			val ret =  server.dispatcher.pilotNameParser.apply(in)
			
			if(ret.successful) {
				Success(ret.asInstanceOf[{def result:String}].result , ret.next)
			}else{
			  Failure("not a known pilot name", ret.next)
			}
    }
 	}
 	def pilotNameParser :  Parser[String] = pilotNameParserObj
 	object memorizingPilotNameParserObj extends Parser[String] {
 		var state = ""
 		def apply(in:Input)={
			val ret =  pilotNameParser.apply(in)
			
			if(ret.successful) {
				state = ret.asInstanceOf[{def result:String}].result
//println("memoing pilot "+state)				
				ret
			}else{
//debug("did not find a pilot for memoing: "+in.source)				
				state = ""
			  ret
			}
    }
 	}
 	def memorizingPilotNameParser :  Parser[String] = memorizingPilotNameParserObj
 	def memoedName :  Parser[String] = {
 		memorizingPilotNameParserObj.state ^^ {memoed =>
println("identified memoed "+memoed) 		
 			memoed
 		}
 	}
 	
 	def learnNewName(pilotName:String):Unit = server.dispatcher.pilotNameParser.add(pilotName)
//  def learnNewName(pilotName:String):Unit = server.dispatcher.pilotNameParser.learnNewName(pilotName)
// 	def learnNewName(pilotName:String):Unit = server.dispatcher.pilotNameParser.add(pilotName, pilotName)
    
 	
  /**
   * teaches pilot names to dispatcherparser...
   */
 	lazy val connectParser : Parser[PilotMessage] = 
    ".* has connected".r ^^ {all=>
      val pilotName = all.substring(0, all.length-14)
      
      learnNewName(pilotName)
//      pilotNameParser.learnNewName(pilotName)
      PilotMessage(pilotName, Is.Joining)
 	} 
  
  case class PlaneAndSeat (plane:String, seat: Int)
  lazy val planeAndSeat : Parser[PlaneAndSeat] = {
  	regexMatch("""\:(\S+)\((\d+)\)""") ^^ { both=>
  		val plane = both.group(1)
  		val num = both.group(2).toInt
  		PlaneAndSeat(plane, num)
  	}
  }
  lazy val seatOccupied : Parser[PilotMessage] = {
    memorizingPilotNameParser ~ planeAndSeat ~" seat occupied by " ~ memoedName ~ atLocationParser ^^ {
    	case pilot ~ both ~ _ ~ _ ~ at => {
      	PilotMessage(pilot, Is.TakingSeat(both.plane), at)
      }
    }
  }
  lazy val ejected : Parser[PilotMessage] = {
    pilotNameParser ~ planeAndSeat ~opt("successfully") ~" bailed out" ~ atLocationParser ^^ {
    	case pilot ~_ ~ _ ~ _ ~ at => {
      	PilotMessage(pilot, Is.Ejecting, at)
      }
    }
  }
  
  lazy val loading : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ " loaded weapons '" ~ """[^']+""".r ~ "' " ~ fuelParser ^^ {
      case pilot ~ _ ~ plane ~ _ ~ loadout ~ _ ~ fuel => {
        PilotMessage(pilot, Is.Loading(plane, loadout, fuel/100)) 
      }
    }
  }

  lazy val inFlight : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ " in flight"  ~ atLocationParser ^^ {
      case pilot ~ _ ~ plane ~ _ ~ at => {
debug("inflight event "+new Exception)      	
        PilotMessage(pilot, Is.InFlight, at) 
      }
    }
  }
  lazy val landed : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ " landed"  ~ atLocationParser ^^ {
      case pilot ~ _ ~ plane ~ _ ~ at => {
        PilotMessage(pilot, Is.Returning, at) 
      }
    }
  }
  lazy val disconnecting : Parser[PilotMessage] = {
    pilotNameParser <~ " has disconnected" ^^ (PilotMessage(_, Is.Leaving)) 
  }
  
  lazy val refly : Parser[PilotMessage] = {
    pilotNameParser <~ " entered refly menu" ^^ (PilotMessage(_, Is.Selecting)) 
  }
  
  lazy val wasKilled : Parser[PilotMessage] = {
    pilotNameParser ~ planeAndSeat ~ " was killed"  ~ atLocationParser ^^ {
      case pilot ~ plane ~ _ ~ at => {
        PilotMessage(pilot, Is.Killed, at) 
      }
    }
  }  

	lazy val atLocationParser : Parser[At.Location] = {
	  " ?at ".r ~ simpleDouble ~" "~ simpleDouble ^^ {
	    case _ ~ x ~ _ ~ y => At.Coordinate(x,y)
	    case _ => At.Nowhere
	  }
	} 
  lazy val simpleDouble : Parser[Double] = {
     """\d+\.\d+""".r ^^ (_ toDouble)
	}
   
}
