package de.immaterialien.sturmonanny.multiplexer

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable 


import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._
 
trait Domain[D <: Domain[D]] extends LiftActor with Logging{
 	self : D =>
	val items : mutable.Map[String, this.Element] = new mutable.LinkedHashMap
	override def messageHandler = {
	  case p : this.Element => items.put(p.name, p)
	  case forward(who, what) => items.get(who).foreach(x=>x ! what)
	  case forAll(what) => items.values.foreach(x=>x ! what)
	  case unregister(p) => items.removeKey(p.name) 
	}

	abstract class Element(val name : String) extends TimedLiftActor with Logging{
		val domain = Domain.this
		def unknownMessage(x : Any) = debug(this.getClass.getSimpleName +" "+name + " got unidentified message "+ x)
	}
  	case class unregister(val who : Element)
  	case class forward(val who : String, val what : Any)
  	case class forAll(what : Any)
}
