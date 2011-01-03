package de.immaterialien.sturmonanny.core


/**
 * implementations run synchronised behind an actor
 * implementations have to have a default constructor
 * implementations receive a custom configuration file path as their configuration after construction 
 *  but possibly also at other times
 */
trait IMarket { import IMarket._
	
	final def getPrice(plane:String, side:Int) : Double = getPrice(plane, None, side)
	final def getPrice(plane:String, load:String, side:Int) : Double = getPrice(plane, Some(load), side)
	final def getPrice(plane:String, load:Option[String], side:Int) : Double = getPrice(Loadout(plane, load), side)
	def getPrice(loadout:Loadout, side:Int) : Double = loadout match {
		case Loadout(_, None) => tryPrice(loadout, side) getOrElse 0D
		case Loadout(plane, Some(_)) => tryPrice(loadout, side) getOrElse getPrice(Loadout(plane, None), side)
	}
	
	def tryPrice(loadout:Loadout, side:Int) : Option[Double]
	

	
	/**
	 * @param plane
	 * @param millis
	 */
	def addAirTime(plane : Loadout, millis : Long, side:Int) : Unit
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
	def cycle(name : java.io.File) : Unit   
}   
object IMarket {
	case class Loadout(plane:String, load:Option[String]) {
		override def toString = load map (plane+"_"+_) getOrElse plane 
	}
}