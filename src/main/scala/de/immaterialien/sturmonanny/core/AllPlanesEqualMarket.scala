package de.immaterialien.sturmonanny.core

/**
 * a dummy market
 */
class AllPlanesEqualMarket extends IMarket{
	def getPrice(plane : String) : Double = 0d
	def addAirTime(plane : String, millis : Long) {}
	def setConfiguration(pathToFile : String) = true
	def cycle = ()
}
