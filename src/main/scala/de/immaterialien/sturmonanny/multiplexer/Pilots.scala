package de.immaterialien.sturmonanny.multiplexer

import _root_.de.immaterialien.sturmonanny.util.{TimedLiftActor, Logging}
import _root_.de.immaterialien.sturmonanny.multiplexer._
import scala.collection.mutable

 
object DIES
case class chats(val msg : String) 
case class join(val side : Army.Army) 
case class inform(val text : String, val to : String)

class Pilots(val multiplexer : Multiplexer) extends Domain[Pilots]{
	class Pilot(override val name : String) extends Pilots.this.Element(name) with Sided{
		val planes = Army Val new mutable.LinkedHashMap[String, Int]
		var died = Army Var 0
		var flies = -1
  
		override def defaultMessageHandler = { 
		  case PERSIST =>

   		  case DIES => {
   		    died(this) = 1 + died(this)
   		    flies = -1
          }
   		  case flies(plane) => planes(this).put(plane, 1+planes(this)(plane)) 
		  case _ => unknownMessage _ 
		}
	}
}
