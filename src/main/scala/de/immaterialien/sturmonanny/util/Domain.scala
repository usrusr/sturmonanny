package de.immaterialien.sturmonanny.util

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
	  case Domain.forward(who, what) => {
debug("forwarding  "+who+" <- "+items+" -> "+what)	    
	    items.getOrElseUpdate(who, newElement(who) )  ! what
	  } 
	  case Domain.forAll(what) => items.values.foreach(x=>x ! what)
	  case unregister(p) => items.removeKey(p.name)
   case x => debug("unknown in domain "+this.getClass.getSimpleName+": "+x)   
	}
  
	abstract class Element(val name : String) extends TimedLiftActor with Logging{
		val domain = Domain.this
		domain ! this
		def unknownMessage(x : Any) = debug(this.getClass.getSimpleName +" "+name + " got unidentified message "+ x)
	}
	def newElement(name:String) : this.Element
 	
	case class unregister(val who : Element)
}
object Domain {
  	case class forward(val who : String, val what : Any)
  	case class forAll(what : Any)
    
}