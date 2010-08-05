package de.immaterialien.sturmonanny.core


/**
 * implementations run synchronised behind an actor
 * implementations have to have a default constructor
 * implementations receive a custom configuration file path as their configuration after construction 
 *  but possibly also at other times
 */
trait IMarket { import IMarket._
	
	final def getPrice(plane:String) : Double = getPrice(plane, None)
	final def getPrice(plane:String, load:String) : Double = getPrice(plane, Some(load))
	final def getPrice(plane:String, load:Option[String]) : Double = getPrice(Loadout(plane, load))
	def getPrice(loadout:Loadout) : Double = loadout match {
		case Loadout(_, None) => tryPrice(loadout) getOrElse 0
		case Loadout(plane, Some(_)) => tryPrice(loadout) getOrElse getPrice(Loadout(plane, None))
	}
	
	protected def tryPrice(loadout:Loadout) : Option[Double]
	

	
	/**
	 * @param plane
	 * @param millis
	 */
	def addAirTime(plane : Loadout, millis : Long) : Unit
	/**
	 * return true for a successful configuration update 
	 */  
	def setConfiguration(pathToFile : String) : Boolean
	
	def setServerContext(server : Server) : Unit
	
	/**
	 * notify market of a new map
	 * 
	 * @param name
	 */
	def cycle(name : String) : Unit   
}   
object IMarket {
	case class Loadout(plane:String, load:Option[String]) {
		override def toString = load map (plane+" "+_) getOrElse plane 
	}
}