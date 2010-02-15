//package de.immaterialien.sturmonanny.core
//
//import net.liftweb.actor.LiftActor
//import de.immaterialien.sturmonanny.util._
//
///**
// * a place for message parsing to grow and evolve without being disturbed by the necessities of console connections, game logic or configuration
// */
//class Dispatcher extends LiftActor with NonUpdatingMember with Logging{   
//	 def fromServer(lines : Seq[String]) {
//	    
//	 }
//     
//val debugWriter = new java.io.FileWriter("dispatcher.out.txt")   
//  
//	 override def messageHandler = new PartialFunction[Any, Unit](){
//	   override def isDefinedAt(x : Any) = {
//		  internal isDefinedAt(x)
//	  }  
//   	
//	override def apply(x : Any) = {
////debug("dispatching... "+ x )	   
//		  internal apply(x)
//	  }
//	
//	 }
//  
//	 val internal :  PartialFunction[Any, Unit] = {
//  	case DispatchLine(line) => stringInternal(line) 
//   }	
//	 val stringInternal :  PartialFunction[Any, Unit] = {
//	   case Dispatcher.pilotsHeader(_) => {
//	     // ignore...
//	   }
//	        
//	   case Dispatcher.pilotFlying(id, name, ping, score, army, plane) => {
//			server.pilots.forElement(name)( _! Is.Flying(plane, Armies.forName(army)) ) 	
//	   }
//       case Dispatcher.playerJoin(who) => {
//debug("dispatch player join '"+who+"'")
//			server.pilots.create(who)
//       }
//       case Dispatcher.missionIsPlaying(what) => {
//debug("new mission '"+what+"'")
//			server.market.cycle(what)
//       }
//       case Dispatcher.chat(who, what) => {
//debug(who+" chats '"+what+"'")
//         	server.pilots.forElement(who){ _ ! Is.Chatting(what)}
//       }
//       case Dispatcher.parserChat(line) => {
////         Dispatcher.scparser.parse(Dispatcher.scparser.statusChat, line) map {ret=>
////         	server.pilots.forElement(ret.who){ _ ! ret.event}
////         }
//       }
//       
////           case Dispatcher.masterChat(cont) => {
////	            case Dispatcher.joinsTheGame(who) => {
////	                debug("dispatch player join '"+who+"'")
////					server.pilots.create(who)
////	       		}
////	       		case Dispatcher.hasLeftTheGame(who) => {
////	       			debug("dispatch player leave '"+who+"'")
////	//				server.pilots ! Pilots.remove(who)
////	       		}
////             
////                case Dispatcher.landed1(who) => {
////                	pilot(who, Is.returns) 
////                }
////                case Dispatcher.landed2(who) => {
////                	pilot(who, Is.returns) 
////                }
////                case Dispatcher.wasKilled(who) => {
////                	pilot(who, Is.destroyed)
////                }
////                case Dispatcher.hasCrashed(who) => {
////                	pilot(who, Is.destroyed)
////                }
////                case Dispatcher.bailedOut(who) => {
////                	pilot(who, Is.destroyed)
////                }
////                case x => debug(" ---> did not understand '"+x+"' in ")
////           	}
//       case Dispatcher.hasLeftTheGame(who) => {
//           debug("dispatch player leave '"+who+"'")
////				server.pilots ! Pilots.remove(who)
//           			pilot(who, Is.Leaving)
//       }
//
//	   case x => {
//  debug(" --- did not understand '"+x+"'")
//if(isDebugEnabled) {
//  debugWriter.append("'"+x+"'\n")
//  debugWriter flush
//}
//
//	   }
//	 }
//	 private def pilot(who:String, msg:Any) = server.pilots.forElement(who)(_!msg)
//}
//
//object Dispatcher {
//  val missionIsPlaying = """Mission\: (.+) is Playing\\n""".r
//  val playerJoin = """socket channel '\d+', ip (\S+)\:(\d+), (.+), is complete created\\n""".r
//  val pilotsHeader = ("""(\\"""+"""u0020N       Name           Ping    Score   Army        Aircraft)\\n""").r
//  val pilotFlying = (
//		  """\\""" + """u0020"""+ 		// intro
//		  """(\d+)\s+"""+ 				// ID number and some blanks
//		  """(\S(?:.{0,16}\S)?)\s+"""+	// name (tolerating blanks and having a maximum length) and some blanks
//		  """(\d+)\s+"""+				// ping and some blanks
//		  """(-?\d+)\s+"""+				// score and some blanks
//		  """\(\d+\)(\S+)\s+"""+		// army with prefixed number (ignored) and some blanks
//		  """.{0,11}\s"""+				// ignore plane registration (with a maximum length, and at least one blank)
//		  """((?:\S.*)?)\\n"""			// plane before end (does it need to tolerate blanks?)
//  	).r
//  val chat = """Chat: (.+): \\t(.*)\\n""".r
//
//                                    
//    //  val hasLeftTheGame = """Chat: --- (.+) has left the game\.\\n""".r
////  val joinsTheGame = """Chat: --- (.+) joins the game\.\\n""".r
////  val wasKilled = """Chat: --- (.+) was killed\.\\n""".r
////  val hasCrashed = """Chat: --- (.+) has crashed\.\\n""".r
////  val bailedOut = """Chat: --- (.+) bailed out\.\\n""".r
////  val landed = """Chat: --- (.+) is (:?on the ground safe and sound)|(:?RTB)\.\\n""".r
////  
// 
//  val hasLeftTheGame = """(.+) has left the game\.""".r
//  val joinsTheGame = """(.+) joins the game\.""".r
//  val wasKilled = """(.+) was killed\.""".r
//  val hasCrashed = """(.+) has crashed\.""".r
//  val crashes = """(.+) crashes\.""".r
//  val crashes2 = """(.+) just crashed!""".r
//  val bailedOut = """(.+) bailed out\.""".r
//  val captured = """(.+) has beed captured by the opposing force\.""".r
//  val landed1 = """(.+) is RTB\.""".r
//  val landed2 = """(.+) is on the ground safe and sound\.""".r
//  
//                                                                                                    
////    val hasLeftTheGame = """Chat: --- (.+) has left the game\.\\n""".r
////    val joinsTheGame = """Chat: --- (.+) joins the game\.\\n""".r
//////    val destroyed = """Chat: --- (.+) (:?was killed)|(?:has crashed)|(?:bailed out)\.\\n""".r
////    val destroyed = """Chat: --- (.+) was killed\.\\n""".r
//////    val hasCrashed = """Chat: --- (.+) has crashed\.\\n""".r
//////    val bailedOut = """Chat: --- (.+) bailed out\.\\n""".r
////    val landed = """Chat: --- (.+) is (:?on the ground safe and sound)|(:?RTB)\.\\n""".r
//                                                                      
//                                              
//  val parserChat = """(Chat: --- .+)\\n""".r                                      
////  val masterChat = """Chat: --- (.+)\\n""".r
////  // Chat: --- test was killed.\n
////  object sets {
////	  val hasLeftTheGame = Set("""has left the game""")
////	  val joinsTheGame = Set("""joins the game""")
////	  val destroyed = Set(
////			  """was killed""",
////			  """has crashed""",
////			  """bailed out"""
////      )
////	  val landed = Set(
////	    """is on the ground safe and sound""",
////	    """is RTB"""
////      )
////  }
//  
//  
//  val seqSeparator = """-------------------------------------------------------\\n""".r;
//  val seqName = """Name\: \\t(.*)\\n""".r
////  val scparser = new ServerChatParser                            
//}
