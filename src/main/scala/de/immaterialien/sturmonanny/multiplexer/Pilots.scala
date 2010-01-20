package de.immaterialien.sturmonanny.multiplexer

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable


object DIES
case class chats(val msg : String) 
case class join(val side : Army.Army)
case class inform(val text : String, val to : String)

class Pilots(val multiplexer : Multiplexer) extends Domain[Pilots]{
	class Pilot(override val name : String) extends this.Element(name) with Sided{
		val planes = Army Val new mutable.LinkedHashMap[String, Int]
		var died = Army Var 0
  
		override def defaultMessageHandler : PartialFunction[Any, Unit] = {
		  case PERSIST =>
//		  case DIES => this side died update (_+1)
//		  case DIES => this side died is = 1 + (this side died is)
		case DIES => died of this side = 1 + (died of this side)
		 // case flies(plane) => this side planes get (p  => p.put(plane,  1 + p.get(plane).getOrElse(0)))
		 case flies(plane) => (planes of this side).put(plane, 1 + ((planes of this side) get plane getOrElse 0)) 
//		 case flies(plane) => {
//		   //this.side(planes).get(p  => p.put(plane,  1 + p.get(plane).getOrElse(0)))
//		   val p = this side planes get
//       
//    	   p.put(plane,  1 + p.get(plane).getOrElse(0))
//    	   
//		  }
		  case _ => unknownMessage _ 
		}
	}
}
