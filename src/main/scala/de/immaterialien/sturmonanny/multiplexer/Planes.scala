package de.immaterialien.sturmonanny.multiplexer

class Planes(val multiplexer : Multiplexer) extends Domain[Planes]{
	class Plane(override val name : String) extends this.Element(name){
	  	override def defaultMessageHandler : PartialFunction[Any, Unit] = {
		  case PERSIST => 
		  case _ => unknownMessage _ 
	  	} 
 	}  
}

