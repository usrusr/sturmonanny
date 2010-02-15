package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.util.parsing.combinator._
import net.liftweb.actor.LiftActor
import scala.io._
import scala.collection.mutable

object LocalizedDispatcher {
  private val propertiesLine = """^\s*(\S*?)[123456789]?\s+(\S.*)$""".r
}
class LocalizedDispatcher extends LiftActor with UpdatingMember with RegexParsers with Logging{
	var serverPath : String= null
	def i18nSuffix =  """\i18n\netmessages.properties"""
	def updateConfiguration = {
	  val newPath = conf.server.serverPath
   
	  if(newPath!=serverPath){
	    init(newPath)
     
	  } 
	}
	def init(newPath:String) = {
	  val i18nPath = newPath+i18nSuffix
	  try{
		  val source = Source.fromFile(i18nPath)// scala.io.BufferedSource.fromFile(i18nPath)
		  var translations = new mutable.HashMap[String, List[String]]()
		  for(line : String<- source.getLines) {

		    line.stripLineEnd match{
			    case LocalizedDispatcher.propertiesLine(name, text) => {
debug("parsed constant '"+name+"' <- '"+text+"'")				      
			      translations.get(name) match {
			        	case None => translations.put(name, List(text))
			        	case Some(existing) => translations.put(name, text::existing)
			      } 
			    }
			    case x => if( ! x.isEmpty)debug("failed to parse '"+x+"'")	 
			  }
		  }
debug("parsed i18n message definitions: "+translations)
    
	      localizedMessageParser = new LocalizedParserInstance(translations)
	      serverPath = newPath
	      debug("parsed i18n message definitions: "+localizedMessageParser.constantsToLocalized)
      }catch{
        case e:Exception=>if(serverPath!=null) error("Failed to parse i18n message definitions from "+i18nPath+":", e)
      }
	} 
	init(serverPath)
 
	override def messageHandler : PartialFunction[Any, Unit] = {	  
	  case DispatchLine(x) => processLine(x)
	  case _ => // ignore 
	}
	def processLine(line:String) : Unit = {
		val parseResult : ParseResult[_] = parse(rootParser, line)
debug("parsed '"+line+"' \n  -> '"+parseResult+"'")  
		parseResult.getOrElse(None) match { 
		      case PilotMessage(who, Is.Ignored) => // ignore
		      case PilotMessage(who, Is.Unknown) => debug("unkown message: '"+line+"'")
		      case PilotMessage(who, what ) => //server.pilots.forElement(who)(_!what)
        case None => 
        case x:AnyRef => debug("got "+x+"\n  "+x.getClass.getSimpleName)
        case x => debug("got value "+x)
		}   
	}
	lazy val rootParser = (
			ignore
		|   channelLine
		|	statusChat
		|	playerChat
		|	flyingLineParser
	)
	
	lazy val playerChat = {
	  literal("Chat: ")~> pilotNameParser ~ (""": \t""" ~> """.*\\n""".r ) ^^ {case(who ~ what)=>PilotMessage(who, Is.Chatting(what.substring(0, what.length-2)))}
	}
 
	val channelRegex = """socket channel '\d+', ip \d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\:\d+, (.+), is complete created\\n""".r
	lazy val channelLine : Parser[PilotMessage]={
	  channelRegex ^^ {x:String => 
	    x match{
	    	case channelRegex(name) => {
	    	  learnNewName(name) 
	    	 
	    	  PilotMessage(name, Is.Joining)
	    	} 
	    	case _ => {
	    	  error("failed to parse socket creation '"+x+"'")
	    	  PilotMessage("", Is.Unknown)
	    	}
	    }
	  }
	}
	val noNamesParser = literal("")
	var namesParser = noNamesParser
	val namesSet = new mutable.HashSet[String]()
	def learnNewName(name:String){
debug("extending namesparser: '"+name+"'")	  
		if( ! namesSet.contains(name)){
			namesParser = 
//				if(namesParser eq noNamesParser) literal(name)
//				else literal(name) ||| namesParser
				if(namesSet.size==0) literal(name)
				else namesSet.foldLeft(literal(name))(_ ||| _)
			namesSet += name
		}
	} 
//	def pilotNameParser : Parser[String] = new Parser[String]{
////debug("retrieving namesparser!")	  
//		override def apply(in : Input) : ParseResult[String] = {
//		  
////debug("applying namesparser!")	  
//		
//		  namesParser apply in
//		}
//	}
	def pilotNameParser = namesParser
 
 	lazy val ignore : Parser[Is.Event] = (
 		literal("-------------------------------------------------------")
 	|	"""\"""+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
 	) ^^^ Is.Unknown

 	lazy val space :Parser[String] = """\s""".r
  
// 	val pilotPattern = """\S(.*\S)?""".r
 	
   	lazy val flyingLineParser : Parser[PilotMessage] = literal("""\""" + """u0020""")  ~> (
   		"""[1234567890 ]{1,8}""".r ~                // number and blanks """       """
 		pilotNameParser ~ 							// name _1
 		(space ~> """\\d""".r) ~ 					// ping _2
 		(space ~> """-?\\d""".r) ~ 					// score _3
 		(space ~ """\(\d+\)""" ~> """\S+""".r) ~	// army _4
 		(space ~> """\S*""".r)						// plane _5
 	 <~ """\n""") ^^ {
 	   case (_ ~ pilot ~ _ ~ _ ~ army ~ plane) => PilotMessage(pilot, Is.Flying(plane, Armies.forName(army)))
 	 } 



  
    var localizedMessageParser : LocalizedParserInstance = null
	def eventParser = localizedMessageParser.pilotEventParser
 
	lazy val statusChat : Parser[PilotMessage] =  literal("Chat: --- ") ~> eventParser
 
 
	 class LocalizedParserInstance(val constantsToLocalized : mutable.Map[String, List[String]]) {
		private def parserForSingleLine(text:String):Option[Parser[String]] = {
			val ignoreOne = text.replaceAll("""\{1\}""", """\E.*\Q """).replaceAll("''", "'")
			def quoted(what:String) = ("""\Q"""+what+"""\E""").r
		   
			val parser : Option[Parser[String]]= ignoreOne match {
				case LocalizedParserInstance.pilotTail(tail) => {
//debug("creating pilotNameParser <~ "+quoted(tail)+ " ")				  
			      Some(pilotNameParser <~ quoted(tail))
			    }
			    case LocalizedParserInstance.headPilotTail(head, tail) => {
//debug("creating "+quoted(head)+" ~> pilotNameParser <~ "+quoted(tail)+ " ")				  
			      Some(quoted(head) ~> pilotNameParser <~ quoted(tail))
			    }
			    case LocalizedParserInstance.headPilot(head) => {
//debug("creating "+quoted(head)+" ~> pilotNameParser ")				  
			      Some(quoted(head) ~> pilotNameParser)
			    }
			    case _ => None
			}
			parser
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
		lazy val gore_kill 		= selectLine("""gore_kill"""		, Is.Dying)
		lazy val gore_killaaa 	= selectLine("""gore_killaaa"""		, Is.Dying)
		lazy val gore_gun 		= selectLine("""gore_gun"""			, Is.Dying)
		lazy val gore_tank 		= selectLine("""gore_tank"""		, Is.Dying)
		lazy val gore_ship 		= selectLine("""gore_ship"""		, Is.Dying)
		lazy val gore_ai 		= selectLine("""gore_ai"""			, Is.Dying)
		lazy val gore_sawwing 	= selectLine("""gore_sawwing"""		, Is.Crashing)
		lazy val gore_blowwing 	= selectLine("""gore_blowwing"""	, Is.Dying)
		lazy val gore_blowup 	= selectLine("""gore_blowup"""		, Is.Dying)
		lazy val gore_lightfuel = selectLine("""gore_lightfuel"""	, Is.Unknown)
		lazy val gore_lighteng 	= selectLine("""gore_lighteng"""	, Is.Unknown)
		lazy val gore_blowtail 	= selectLine("""gore_blowtail"""	, Is.Crashing)
		lazy val gore_pk 		= selectLine("""gore_pk"""			, Is.Dying)
		lazy val gore_pk30 		= selectLine("""gore_pk30"""		, Is.Dying)
		lazy val gore_headshot 	= selectLine("""gore_headshot"""	, Is.Dying)
		lazy val gore_throat 	= selectLine("""gore_throat"""		, Is.Dying)
		lazy val gore_gk 		= selectLine("""gore_gk"""			, Is.Dying)
		lazy val gore_gheadshot = selectLine("""gore_gheadshot"""	, Is.Dying)
		lazy val gore_hitctrls 	= selectLine("""gore_hitctrls"""	, Is.Unknown)
		lazy val gore_crashes 	= selectLine("""gore_crashes"""		, Is.Crashing)
		lazy val gore_lands 	= selectLine("""gore_lands"""		, Is.Returning)
		lazy val gore_crashland = selectLine("""gore_crashland"""	, Is.Crashing)
		lazy val gore_ditch 	= selectLine("""gore_ditch"""		, Is.Crashing)
		lazy val gore_swim 		= selectLine("""gore_swim"""		, Is.Crashing)
		lazy val gore_pkonchute = selectLine("""gore_pkonchute"""	, Is.Dying)
		lazy val gore_spins 	= selectLine("""gore_spins"""		, Is.Crashing)
		lazy val gore_spinfatal = selectLine("""gore_spinfatal"""	, Is.Dying)
		lazy val gore_rocketed 	= selectLine("""gore_rocketed"""	, Is.Dying)
		lazy val gore_bombed 	= selectLine("""gore_bombed"""		, Is.Dying)
		lazy val gore_walkaway 	= selectLine("""gore_walkaway"""	, Is.Crashing)
		lazy val gore_burnedcpt = selectLine("""gore_burnedcpt"""	, Is.Dying)
		lazy val gore_hitouttac = selectLine("""gore_hitouttac"""	, Is.Crashing)
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
