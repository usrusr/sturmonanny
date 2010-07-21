package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
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
 
 	def updateConfiguration = {

	}

 	def pilotMessageSend(who:String, what: Is.Event) = server.pilots.forElement(who)(_!what)
	override def messageHandler : PartialFunction[Any, Unit] = {	  
	  case DispatchLine(x) => processLine(x)
	  case DispatchMessage(x) => processMessage(x)
	  case _ => // ignore 
	}

	def processLine(line:String) : Unit = {
		val parseResult : ParseResult[_] = parse(rootLineParser, line.stripLineEnd+"\n")
//debug("-------------------------\nparsing line '"+line+"' \n  -> '"+parseResult+"'")  
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
          server.market.cycle(mis)
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
//debug("=============================\nparsing message '"+lines+"' \n  -> '"+parseResult+"'")  
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
//debug("success from message: "+who+" -> "+what+"  from '"+lines+"'")		        
		       }
		  }
		}
	}
 
  	lazy val rootMessageParser : Parser[Seq[Message]]= ( 
			(lineParser <~ "\n")^^(lineResult=>List(lineResult))
	)
  
	lazy val rootLineParser = (
			lineParser | (".*".r^^^ Is.Ignored)
	)
	lazy val lineParser : Parser[Message]= (
	  dateTimeParser ~> eventParser
	)
	lazy val dateTimeParser : Parser[java.util.Calendar] = (
	  "[" ~ dayParser  ~ hmsParser ~ ":" ~ hmsParser~":" ~ hmsParser ~ (" AM" ^^^ 0 | " PM" ^^^ 12) ~ "] " 
	  	^^ { case  _ ~ day ~ hour ~_~ minute ~_~ second ~ amPm ~ _  =>  
	  		import java.util.Calendar
	  	  val x:java.util.Date = day
	  	  val offs : Int = amPm
	  	  val ret = Calendar.getInstance
	  	  ret setTime day
	  	  ret set(Calendar.HOUR, hour + amPm)
	  	  ret set(Calendar.MINUTE, minute)
	  	  ret set(Calendar.SECOND, second)
	  	  ret
	  }
	)

  var dayState : java.util.Date = _
  val dateformat = new java.text.SimpleDateFormat("MMM d, yyyy")
 	lazy val dayParser : Parser[java.util.Date] = {
    """\w{3} \d{1,2}, \d{4} """.r ^^ 
    {string=>
      dayState = dateformat.parse(string)
      dayState
    } | ("" ^^^ dayState)
 	}

	lazy val hmsParser : Parser[Int] = {
	  """\d\d""".r ^^ (_ toInt)
	}
	lazy val fuelParser : Parser[Int] = {
	  "fuel "~> """\d+""".r <~"%" ^^ (_ toInt)
	}
 	lazy val eventParser : Parser[Message] = (
		  "Mission BEGIN" ^^^ GlobalMessage(Is.MissionBegin)
    | "Mission END" ^^^ GlobalMessage(Is.MissionEnd)
    | ("Mission: " ~> ".*".r <~ ".mis is Playing") ^^ (x => GlobalMessage(Is.MissionChanging(x+".mis")))
    | connectParser
    | loading
      	|	seatOccupied
        | inFlight
        | landed  
        | wasKilled  
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
  
  object pilotNameParser extends Parser[String] {
    def apply(in:Input)={
			val ret =  server.dispatcher.pilotNameParser.apply(in)
			
			if(ret.successful) {
				Success(ret.asInstanceOf[{def result:String}].result , ret.next)
			}else{
			  Failure("not a known pilot name", ret.next)
			}
    }
    def learnNewName(pilotName:String) = server.dispatcher.pilotNameParser.learnNewName(pilotName)
    
  }
  /**
   * teaches pilot names to dispatcherparser...
   */
 	lazy val connectParser : Parser[PilotMessage] = 
    ".* has connected".r ^^ {all=>
      val pilotName = all.substring(0, all.length-14)
      
      pilotNameParser.learnNewName(pilotName)
//      pilotNameParser.learnNewName(pilotName)
      PilotMessage(pilotName, Is.Joining)
 	} 
  
  lazy val planeNumber : Parser[Int] = {
    "(" ~> """\d+""".r <~ ")" ^^ (_ toInt)
  }
  lazy val seatOccupied : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ planeNumber~" seat occupied by " ~ pilotNameParser ~ atLocationParser ^^ {
      case pilot ~ _ ~ plane ~ planeNum ~ _ ~ p2 ~ at if pilot==p2 => {
        PilotMessage(pilot, Is.TakingSeat(plane), at) 
      }
    }
  }
  
  lazy val loading : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ " loaded weapons '" ~ """\S+""".r ~ "'" ~ fuelParser ^^ {
      case pilot ~ _ ~ plane ~ _ ~ loadout ~ _ ~ fuel => {
        PilotMessage(pilot, Is.Loading(plane, loadout, fuel/100)) 
      }
    }
  }

  lazy val inFlight : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ " in flight "  ~ atLocationParser ^^ {
      case pilot ~ _ ~ plane ~ _ ~ at => {
        PilotMessage(pilot, Is.InFlight, at) 
      }
    }
  }
  lazy val landed : Parser[PilotMessage] = {
    pilotNameParser ~ ":" ~ """\S+""".r ~ " landed "  ~ atLocationParser ^^ {
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
    pilotNameParser ~ ":" ~ """\S+""".r ~ planeNumber ~ " was killed "  ~ atLocationParser ^^ {
      case pilot ~ _ ~ plane ~ _ ~ _ ~ at => {
        PilotMessage(pilot, Is.Dying, at) 
      }
    }
  }  

	lazy val atLocationParser : Parser[At.Location] = {
	  " at " ~ simpleDouble ~" "~ simpleDouble ^^ {
	    case _ ~ x ~ _ ~ y => At.Coordinate(x,y)
	    case _ => At.Nowhere
	  }
	} 
  lazy val simpleDouble : Parser[Double] = {
     """\d+\.\d+""".r ^^ (_ toDouble)
	}
   
}
