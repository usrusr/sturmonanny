package de.immaterialien.sturmonanny.multiplexer

class Planes extends Domain[Planes] with NonUpdatingMember{
	class Plane(override val name : String) extends this.Element(name){
	  	override def defaultMessageHandler : PartialFunction[Any, Unit] = {
		  case PERSIST => 
		  case _ => unknownMessage _ 
	  	} 
 	}  
} 

