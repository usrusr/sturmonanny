package de.immaterialien.sturmonanny.core

/**
 * a dummy market
 */
class AllPlanesEqualMarket extends IMarket{
	def tryPrice(plane : IMarket.Loadout, side:Int) : Option[Double] = Some(0d)
	def addAirTime(plane : IMarket.Loadout, millis : Long, side:Int) {}
	def setConfiguration(pathToFile : String) = true
	def cycle(mission : java.io.File) = ()
	def setServerContext(server : Server) : Unit = ()
}
