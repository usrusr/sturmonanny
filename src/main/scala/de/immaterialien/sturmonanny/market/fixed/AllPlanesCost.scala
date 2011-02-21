package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny.core._

class AllPlanesCost extends IMarket{ import IMarket._
	def tryPrice(loadout:Loadout, side:Int) : Option[Double] = Some(1000D)
	var serverOpt : Option[Server] = None 

println("creating all planes cost market")	
	/**
	 * @param plane
	 * @param millis
	 */
	def addAirTime(plane : Loadout, millis : Long, side:Int) : Unit =()
	/**
	 * return true for a successful configuration update 
	 */  
	def setConfiguration(pathToFile : String) : Boolean = true
	
	def setServerContext(srv : Server) : Unit = if(srv!=null) (serverOpt=Some(srv))
	
	def cycle(name : java.io.File) = {
	 
		for(
				set<-AvailablePlanesIdentifier.cycle(name).getOrElse(Map()).values;
				plane<-set;
				srv <- serverOpt
				) {
//debug("creating "+plane+" in market ")
				srv.planes.create(plane)

		}
//		def dateFloor(date:String) = date+("0"*(9-math.min(date.length, 9))) 
		
	}
}