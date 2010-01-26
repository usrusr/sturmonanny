package de.immaterialien.sturmonanny.multiplexer


/**
 * implementations run synchronised behind an actor
 * implementations have to have a default constructor
 * implementations receive a custom configuration file path as their configuration after construction 
 *  but possibly also at other times
 */
trait IMarket {
	def getPrice(plane : String) : Double
	def addAirTime(plane : String, millis : Long) : Unit
	/**
	 * return true for a successful configuration update 
	 */
	def setConfiguration(pathToFile : String) : Boolean
}
