package de.immaterialien.sturmonanny.multiplexer

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable


class Pilots(val multiplexer : Multiplexer) extends Domain[Pilots]{
	class Pilot(override val name : String) extends this.Element(name){
		val planes : mutable.Map[String, Int] = new mutable.LinkedHashMap
		override def defaultMessageHandler : PartialFunction[Any, Unit] = {
		  case PERSIST => 
		  case _ => unknownMessage _ 
		}
	}
}
