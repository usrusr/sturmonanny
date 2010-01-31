package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util.Domain

class Planes extends Domain[Planes] with NonUpdatingMember{ 
	class Plane(override val name : String) extends this.Element(name){
	  	override def defaultMessageHandler : PartialFunction[Any, Unit] = {
		  case PERSIST => 
		  case _ => unknownMessage _  
	  	} 
 	}    
}  

 