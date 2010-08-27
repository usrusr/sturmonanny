package de.immaterialien.sturmonanny.core

/**
 * a dummy market
 */
class AllPlanesEqualMarket extends IMarket{
	def tryPrice(plane : IMarket.Loadout) : Option[Double] = Some(0d)
	def addAirTime(plane : IMarket.Loadout, millis : Long) {}
	def setConfiguration(pathToFile : String) = true
	def cycle(mission : String) = ()
	def setServerContext(server : Server) : Unit = ()
}
