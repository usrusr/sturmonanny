package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util.Domain

class Planes extends Domain[Planes] with NonUpdatingMember{
	def time = server.time
	override def newElement(name:String) = new Plane(name)
	class Plane(override val name : String) extends this.Element(name){
	  	override def messageHandler : PartialFunction[Any, Unit] = {
		  case Is.Persisted => 
		  case _ => unknownMessage _   
	  	} 
 	}     
}  

  