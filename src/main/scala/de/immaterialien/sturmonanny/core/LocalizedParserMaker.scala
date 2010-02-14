package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import scala.util.parsing.combinator._

class LocalizedParserMaker extends UpdatingMember with RegexParsers with Logging{
	var serverPath = ".."
	def i18path = serverPath + """\i18n\netmessages.properties"""
	def updateConfiguration = {
	  val newPath = conf.server.serverPath
   
	  if(newPath!=serverPath){
	    
	  }
	}
	def init = {
	  
	}
	init
 
    var localizedMessageParser : LocalizedParserInstance = null
	def eventParser = localizedMessageParser.pilotEventParser
 
	lazy val statusChat : Parser[PilotEvent] =  "Chat: --- " ~> eventParser
 
 
	 class LocalizedParserInstance(val map : Map[String, Seq[String]]) {
		private def parserForSingleLine(text:String):Option[Parser[String]] = {
		  val ignoreOne = text.replaceAll("""\{1\}""", """\E.*\Q """)
			  def quoted(what:String) = ("""\Q"""+what+"""\E""").r
		   
			  val parser : Option[Parser[String]]= ignoreOne match {
			    case LocalizedParserInstance.pilotTail(pilot, tail) => {
			      Some(pilot <~ quoted(tail))
			    }
			    case LocalizedParserInstance.headPilotTail(head, pilot, tail) => {
			      Some(quoted(head) ~> pilot <~ quoted(tail))
			    }
			    case LocalizedParserInstance.headPilot(head, pilot) => {
			      Some(quoted(head) ~> pilot)
			    }
			    case _ => None
			  }
			  parser
		}
		private def mergeLineOptions(name:String):Parser[String]={
		  val texts = map(name)
		  val parsers : Seq[Option[Parser[String]]]= texts.map(parserForSingleLine(_))
	   
		  val list : List[Parser[String]]= (parsers.filter(_ isDefined).map(_ get)).toList
		  val ret : Parser[String] = list match {
		    case head :: Nil => head 
		    case head :: tail => tail.foldLeft(head){(x, y)=> x | y}   
		    case Nil => failure("no text defined for '"+name+"'")
		  }
		  ret
		}
		private def selectLine(name:String, event:Is.Event):Parser[PilotEvent]={
		  mergeLineOptions(name) ^^ (PilotEvent(_, event))
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
	 
	 
		// merge them into one parser
		lazy val pilotEventParser : Parser[PilotEvent]= ( 
				user_joins
			|	user_leaves  	
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
		)	
	 
	 }	
	object LocalizedParserInstance{
	  protected[LocalizedParserMaker] val pilotTail = """^\{0\}(.*)$""".r
	  protected[LocalizedParserMaker] val headPilotTail = """^(.*)\{0\}(.*)$""".r
	  protected[LocalizedParserMaker] val headPilot = """^(.*)\{0\}(.*)$""".r
	}
}
