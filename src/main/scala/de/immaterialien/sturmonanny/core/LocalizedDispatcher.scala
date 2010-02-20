package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.util.parsing.combinator._
import net.liftweb.actor.LiftActor
import scala.io._
import scala.collection.mutable
import scala.util.matching.Regex
//import java.util.regex.Pattern

object LocalizedDispatcher {

  private val PILOTNAMETIMEOUT = 60*1000*1000
}
class LocalizedDispatcher extends LiftActor with UpdatingMember with RegexParsers with Logging with I18nReader{
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
	  val newPath = conf.server.serverPath
   
	  if(newPath!=serverPath){
	    init(newPath)
     
	  } 
	}
 	override def updateTranslations(translations:mutable.Map[String, List[String]]){
    	  localizedMessageParser = new LocalizedParserInstance(translations)
	      debug("parsed i18n message definitions: "+localizedMessageParser.constantsToLocalized)       
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
		parseResult.getOrElse(None) match { 
		      case PilotMessage(who, Is.Ignored) => // ignore
		      case PilotMessage(who, Is.Unknown) => debug("unkown message: '"+line+"'")
		      case PilotMessage(who, what ) => {
		    	  pilotMessageSend(who, what)
//debug("success "+who+" -> "+what+"  from '"+line+"'")		        
		       }
        case None => {
debug("None  from '"+line+"'")		        
        }
          
        case Is.Ignored =>
        case x:AnyRef => debug("got  "+x+"\n  "+x.getClass.getSimpleName+"\n   from '"+line+"'")
        case x => debug("got value "+x)
		}   
	}
 
 	def processMessage(lines:String) : Unit = {
		val parseResult  = this.parseAll(rootMessageParser, lines+"\n")
//debug("=============================\nparsing message '"+lines+"' \n  -> '"+parseResult+"'")  
		val resList = parseResult getOrElse List() 
		for (res <- resList) { 
		  res match { 
		      case PilotMessage(who, Is.Ignored) => // ignore
		      case PilotMessage(who, Is.Unknown) => debug("unkown message: '"+lines+"'")
		      case PilotMessage(who, what ) => {
		    	  pilotMessageSend(who, what)
debug("success from message: "+who+" -> "+what+"  from '"+lines+"'")		        
		       }
		  }
		}
	}
 
  	lazy val rootMessageParser : Parser[Seq[PilotMessage]]= (
			(rep(lineParser <~ "\n")) ~ statParser ~ (rep(lineParser <~ "\n"))  ^^ {
//			  case (None ~ s ~ None)=> s
//			  case (Some(h) ~ s ~ None)=> h ::: s
//			  case (None ~ s ~ Some(t))=> s ::: t
//			  case (Some(h) ~ s ~ Some(t))=> (h ::: s) ::: t
				case (h ~ s ~ t)=> h ::: s ::: t                     
			}
//			| (opt(rep(lineParser <~ "\n")) ~ (pilotsHeader ~ statsLineEnd ~> rep(flyingLineParser)) ~ opt(rep(lineParser <~ "\n")))^^ {
//				  case (None ~ s ~ None)=> s
//				  case (Some(h) ~ s ~ None)=> h ::: s
//				  case (None ~ s ~ Some(t))=> s ::: t
//				  case (Some(h) ~ s ~ Some(t))=> (h ::: s) ::: t
//			}
			| ((rep(lineParser <~ "\n")) ~ (pilotsHeader ~ statsLineEnd ~> rep(flyingLineParser)) ~ (rep(lineParser <~ "\n")))^^ {
				case (h ~ s ~ t)=> h ::: s ::: t
			}
			| (rep(lineParser <~ "\n") <~ consoleNLine)
	)
  
	lazy val rootLineParser = (
			ignore
		|	lineParser
	)
	lazy val lineParser : Parser[PilotMessage]= (
 			playerChat 
		//|	flyingLineParser 
		|	statusChat 
		|   channelLine
		|	consoleNLine
	)
	lazy val statsLineEnd = "\\n\n"
	lazy val consoleNLine = ("""<consoleN><\d+>""".r<~"\n") ^^^ PilotMessage("", Is.Ignored) 
	lazy val statParser = {
 		rep(
	 		separatorLine ~statsLineEnd  ~>
	 		makeStatParserString("""Name""") ~ 
			makeStatParserLong("""Score""") ~
			( makeStatParserString("""State""") <~ 
				makeStatParserLong("""Enemy Aircraft Kill""") ~
				makeStatParserLong("""Enemy Static Aircraft Kill""") ~
				makeStatParserLong("""Enemy Tank Kill""") ~
				makeStatParserLong("""Enemy Car Kill""") ~
				makeStatParserLong("""Enemy Artillery Kill""") ~
				makeStatParserLong("""Enemy AAA Kill""") ~
				makeStatParserLong("""Enemy Wagon Kill""") ~
				makeStatParserLong("""Enemy Ship Kill""") ~
				makeStatParserLong("""Friend Aircraft Kill""") ~
				makeStatParserLong("""Friend Static Aircraft Kill""") ~
				makeStatParserLong("""Friend Tank Kill""") ~
				makeStatParserLong("""Friend Car Kill""") ~
				makeStatParserLong("""Friend Artillery Kill""") ~
				makeStatParserLong("""Friend AAA Kill""") ~
				makeStatParserLong("""Friend Wagon Kill""") ~
				makeStatParserLong("""Friend Ship Kill""") ~
				makeStatParserLong("""Fire Bullets""") ~
				makeStatParserLong("""Hit Bullets""") ~
				makeStatParserLong("""Hit Air Bullets""") ~
				makeStatParserLong("""Fire Roskets""") ~
				makeStatParserLong("""Hit Roskets""") ~
				makeStatParserLong("""Fire Bombs""") ~
				makeStatParserLong("""Hit Bombs""") 
				^?  ( stringToState,"'"+_+"' is not a known pilot state!" )
			) ^^ { case name ~ _ ~ state => PilotMessage(name, state) }
		)  <~ separatorLine ~statsLineEnd
	}
 	def makeStatParserLong(intro:String ):Parser[Long] = {
 		literal(intro+""": """)~rep1("""\t""")~> number <~ statsLineEnd
	}
 	def makeStatParserString (what:String) : Parser[String] = {
 	  (what+""": """)~rep1("""\t""") ~> regexMatch("""(\S(.*\S)?)\\n\n""".r)^^(_.group(1))
	}
 	def stringToState : PartialFunction[String, Is.PilotState] = {
 	  case """Fyling""" => Is.InFlight 
 	  case """Landed at Airfield""" => Is.LandedAtAirfield
 	  case """KIA""" => Is.KIA
 	  case """Hit the Silk""" => Is.HitTheSilk  
 	  case """Selects Aircraft""" => Is.Selecting 
 	}
	
 
 
 
	lazy val playerChat = {
	  literal("Chat: ")~> pilotNameParser ~ (""": \t""" ~> """.*\\n""".r ) ^^ {case(who ~ what)=>PilotMessage(who, Is.Chatting(what.substring(0, what.length-2)))}
	}
 
	val channelRegex = """socket channel '\d+', ip \d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\:\d+, (.+), is complete created\\n""".r
	lazy val channelLine : Parser[PilotMessage]={
	  channelRegex ^^ {x:String => 
	    x match{
	    	case channelRegex(name) => {
	    	  pilotNameParser.learnNewName(name) 
	    	 
	    	  PilotMessage(name, Is.Joining)
	    	} 
	    	case _ => {
	    	  error("failed to parse socket creation '"+x+"'")
	    	  PilotMessage("", Is.Unknown)
	    	}
	    }
	  }
	}
 
	
	/**
     * a dynamic token set (with recently-used timeout) that happens to 
     * 
     */
  	object pilotNameParser extends Parser[String] {
		private val noNamesParser = literal("")
		private var namesParser : Parser[String]= noNamesParser
		private val namesSet = new mutable.LinkedHashMap[String, TimeOutingLiteral]()
  
		class TimeOutingLiteral(val string:String, var lastUse:Long) extends Parser[String] {
			val inner = literal(string)
			override def apply(in:Input) = {
				lastUse = System.currentTimeMillis
				inner.apply(in) 
			}
		}
  
		def learnNewName(name:String){
			if( ! namesSet.contains(name)){
				val now = System.currentTimeMillis
				val newParser = new TimeOutingLiteral(name, now)
				val newParserAsStringParser : Parser[String]= newParser 
				val minLastUse = now - LocalizedDispatcher.PILOTNAMETIMEOUT
    
				// cleanup
				namesSet.retain((_, p)=>p.lastUse > minLastUse)
    
				// longest match or first match? 
				// we take first match, so there is no possible way to cut off an existing player from commands 
				namesParser = 
					if(namesSet.size==0) newParser
					else namesSet.values.foldRight(newParserAsStringParser){
					    (older : TimeOutingLiteral, newer : Parser[String])=> older | newer
					}
					
				namesSet.put(name, newParser)
//debug("extended namesparser with '"+name+"', new size is "+namesSet.size )	  
			}
		} 
	
		override def apply(in:Input) ={
			val ret=  namesParser.apply(in)
debug("pilot names parser applying for "+namesSet.keys.mkString("|")+ " -> "+ret)   
			ret
		}
	}
	lazy val separatorLine = literal("-------------------------------------------------------")
	lazy val pilotsHeader = literal("""\"""+"""u0020N       Name           Ping    Score   Army        Aircraft""")
 	lazy val ignore : Parser[Is.Event] = (
 		separatorLine
 	|	pilotsHeader ~ """\n"""
 	) ^^^ Is.Ignored

 	lazy val space :Parser[String] = """\s+""".r
	lazy val positiveNumber = """\d+""".r ^^ (_ toLong)
	lazy val number = """-?\d+""".r ^^ (_ toLong)


   	/**
   	 * special parsing of the "pilot line" that should be very robust against malicious pilot names
   	 * since it reads the string back to front, using the (n)Army part as a landmark
   	 * 
   	 * consider this as a little gem of imperative programming in all the functional sweetness of parser combinators ;-)
   	 */
	lazy val flyingLineParser : Parser[PilotMessage] = ("""^\\""" + """u0020(.*)\\n\n""").r ^? new PartialFunction[String, PilotMessage]{
 		 case class Last(in:String, out:Option[PilotMessage])
 		 def isDefinedAt(x:String) = {
 			 internal(x).isDefined
 		 }
 		 def apply(x:String) = {
 			 internal(x).get
 		 }
 		 var last : (String,Option[PilotMessage]) = null // memoizing with a "map" of size = 1
 		 def internal(x:String) : Option[PilotMessage]={
 			val mem = last
 			if(mem!=null && (mem._1 eq x) ){
 				mem _2
 			}else{
 				
 				val nMem = (x, unbufferedInternal(x.substring(13, x.stripLineEnd.length-2)))
 				last = nMem
 				nMem _2
 			}
 		 }

 		 val idPilotPingPoints = """\s+(\d+-?)\s+(\d+)\s+(\S.*\S)\s*$""".r
 		 val sideNone = ("""^([ABCDEFGHIJKLMNOPQRSTUVWXYZ]\w+)\s+$""").r
 		 val sidePlane = ("""^([ABCDEFGHIJKLMNOPQRSTUVWXYZ]\w+).*\s+(\S+)$""").r
 		 def unbufferedInternal(x:String) : Option[PilotMessage] = {
 			var i = x.length
 			var ret : Option[PilotMessage] = null
 			while(ret==null){
	 		    i =x.lastIndexOf(')', i-1)
	 		    ret = if(i<0){
	 		    	debug("failed to read '"+x+"'")
	 		    	None
	 		    }else{
		 		    val after=x.substring(i+1)
		 		    val before = x.substring(0,i-2)
					val rbefore : String = before.reverse
//debug("trying to read '"+before+"' (..) '"+after+"'")
					rbefore match {
						case idPilotPingPoints(rpo, rpi, rpilot) => {
							val pilot=rpilot.reverse
							after match {
								case sideNone(side) => Some(PilotMessage(pilot, Is.Flying("", Armies.forName(side))))
								case sidePlane(side, plane) => Some(PilotMessage(pilot, Is.Flying(plane, Armies.forName(side))))
								case _ => null
							}
						}
						case _ =>{
//debug("before fail '"+rbefore+"'") 
							null
						}
					}
		 		}
	 		    ()
 			}
 			for(pmsg<-ret) pilotNameParser.learnNewName(pmsg.who)
//debug("pilot line matched to "+ret)
 			ret
 		 }
 	} 	



    var localizedMessageParser : LocalizedParserInstance = null
	def eventParser = localizedMessageParser.pilotEventParser
 
	lazy val statusChat : Parser[PilotMessage] =  literal("Chat: --- ") ~> eventParser
 
 
	 class LocalizedParserInstance(val constantsToLocalized : mutable.Map[String, List[String]]) {
		private def parserForSingleLine(text:String):Option[Parser[String]] = {
				
			val ignoreOne = text.replaceAll("""\{1\}""", """\E.*\Q """).replaceAll("''", "'")
			val findZero = ignoreOne.replaceAll("""\{0\}""", """\\E(.*)\\Q""")
			
			if(findZero==ignoreOne) {
				None
			}else{
				def quoted(what:String) = ("""\Q"""+what+"""\E""").r
				val regex = quoted(findZero)

				Some(
					regexMatch(regex) ^^ { (m : Regex.Match) =>
//debug(" regex match result 0'"+m.group(0)+"' 1'"+m.group(1)+"'" )
						m.group(1)
					}
				) 
			}
		}
		private def mergeLineOptions(name:String):Parser[String]={
		  val texts = constantsToLocalized(name)
//debug("found "+name+" -> "+texts+ " ")    
		  val parsers : Seq[Option[Parser[String]]]= texts.map(parserForSingleLine(_))
	   
		  val list : List[Parser[String]]= (parsers.filter(_ isDefined).map(_ get)).toList
		  val ret : Parser[String] = list match {
		    case head :: Nil => head 
		    case head :: tail => tail.foldLeft(head){(x, y)=> x | y}   
		    case Nil => failure("no text defined for '"+name+"'")
		  }
		  ret
		}
		private def selectLine(name:String, event:Is.Event):Parser[PilotMessage]={
		  mergeLineOptions(name) ^^ (PilotMessage(_, event))
		}
	 
		// a few straight forward copy/paste definitions
	  
		lazy val user_joins 	= selectLine("""user_joins""" 		, Is.Joining)
		lazy val user_leaves 	= selectLine("""user_leaves""" 		, Is.Leaving)
		lazy val user_kicked 	= selectLine("""user_kicked""" 		, Is.Leaving)
		lazy val user_timeouts 	= selectLine("""user_timeouts"""	, Is.Leaving)
		lazy val user_joinarmy 	= selectLine("""user_joinarmy"""	, Is.Unknown)
		lazy val user_readytogo = selectLine("""user_readytogo"""	, Is.Unknown)
		lazy val user_cheating 	= selectLine("""user_cheating"""	, Is.Unknown)
		lazy val user_cheatkick = selectLine("""user_cheatkick"""	, Is.Leaving)
		lazy val gore_kill 		= selectLine("""gore_kill"""		, Is.Unknown)
		lazy val gore_killaaa 	= selectLine("""gore_killaaa"""		, Is.Dying)
		lazy val gore_gun 		= selectLine("""gore_gun"""			, Is.Dying)
		lazy val gore_tank 		= selectLine("""gore_tank"""		, Is.Dying)
		lazy val gore_ship 		= selectLine("""gore_ship"""		, Is.Dying)
		lazy val gore_ai 		= selectLine("""gore_ai"""			, Is.Dying)
		lazy val gore_sawwing 	= selectLine("""gore_sawwing"""		, Is.Unknown)
		lazy val gore_blowwing 	= selectLine("""gore_blowwing"""	, Is.Unknown)
		lazy val gore_blowup 	= selectLine("""gore_blowup"""		, Is.Unknown)
		lazy val gore_lightfuel = selectLine("""gore_lightfuel"""	, Is.Unknown)
		lazy val gore_lighteng 	= selectLine("""gore_lighteng"""	, Is.Unknown)
		lazy val gore_blowtail 	= selectLine("""gore_blowtail"""	, Is.Unknown)
		lazy val gore_pk 		= selectLine("""gore_pk"""			, Is.Unknown)
		lazy val gore_pk30 		= selectLine("""gore_pk30"""		, Is.Unknown)
		lazy val gore_headshot 	= selectLine("""gore_headshot"""	, Is.Unknown)
		lazy val gore_throat 	= selectLine("""gore_throat"""		, Is.Unknown)
		lazy val gore_gk 		= selectLine("""gore_gk"""			, Is.Unknown)
		lazy val gore_gheadshot = selectLine("""gore_gheadshot"""	, Is.Unknown)
		lazy val gore_hitctrls 	= selectLine("""gore_hitctrls"""	, Is.Unknown)
		lazy val gore_crashes 	= selectLine("""gore_crashes"""		, Is.Crashing)
		lazy val gore_lands 	= selectLine("""gore_lands"""		, Is.Returning)
		lazy val gore_crashland = selectLine("""gore_crashland"""	, Is.Crashing)
		lazy val gore_ditch 	= selectLine("""gore_ditch"""		, Is.Crashing)
		lazy val gore_swim 		= selectLine("""gore_swim"""		, Is.Crashing)
		lazy val gore_pkonchute = selectLine("""gore_pkonchute"""	, Is.Unknown)
		lazy val gore_spins 	= selectLine("""gore_spins"""		, Is.Crashing)
		lazy val gore_spinfatal = selectLine("""gore_spinfatal"""	, Is.Dying)
		lazy val gore_rocketed 	= selectLine("""gore_rocketed"""	, Is.Unknown)
		lazy val gore_bombed 	= selectLine("""gore_bombed"""		, Is.Unknown)
		lazy val gore_walkaway 	= selectLine("""gore_walkaway"""	, Is.Crashing)
		lazy val gore_burnedcpt = selectLine("""gore_burnedcpt"""	, Is.Dying)
		lazy val gore_hitouttac = selectLine("""gore_hitouttac"""	, Is.Unknown)
		lazy val gore_vulcher 	= selectLine("""gore_vulcher"""		, Is.Unknown)

		lazy val gore_captured 	= selectLine("""gore_captured"""	, Is.Dying)
		lazy val gore_bailedout = selectLine("""gore_bailedout"""	, Is.Crashing)
		lazy val gore_killed 	= selectLine("""gore_killed"""		, Is.Dying)



	 
		// merge them into one parser
		lazy val pilotEventParser : Parser[PilotMessage]= ( 
				user_leaves  	
//				user_joins
//			|	user_leaves  	
			|	user_kicked 	
			|	user_timeouts 	
			|	user_joinarmy 	
			|	user_readytogo 
			|	user_cheating 	
			|	user_cheatkick 
			|	gore_kill 		
			|	gore_killaaa 	
			|	gore_gun 		
			|	gore_tank 		
			|	gore_ship 		
			|	gore_ai 		
			|	gore_sawwing 	
			|	gore_blowwing 	
			|	gore_blowup 	
			|	gore_lightfuel 
			|	gore_lighteng 	
			|	gore_blowtail 	
			|	gore_pk 		
			|	gore_pk30 		
			|	gore_headshot 	
			|	gore_throat 	
			|	gore_gk 		
			|	gore_gheadshot 
			|	gore_hitctrls 	
			|	gore_crashes 	
			|	gore_lands 	
			|	gore_crashland 
			|	gore_ditch 	
			|	gore_swim 		
			|	gore_pkonchute 
			|	gore_spins 	
			|	gore_spinfatal 
			|	gore_rocketed 	
			|	gore_bombed 	
			|	gore_walkaway 	
			|	gore_burnedcpt 
			|	gore_hitouttac 
			|	gore_vulcher 		  

			|	gore_captured
			|	gore_bailedout 
			|	gore_killed 	


			|	user_joins

		)	
	 
	 }	
	object LocalizedParserInstance{
	  protected[LocalizedDispatcher] val pilotTail = """^\{0\}(.*)$""".r
	  protected[LocalizedDispatcher] val headPilotTail = """^(.*)\{0\}(.*)$""".r
	  protected[LocalizedDispatcher] val headPilot = """^(.*)\{0\}(.*)$""".r
	}
}
