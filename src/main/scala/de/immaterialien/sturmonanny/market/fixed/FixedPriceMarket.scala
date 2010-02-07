package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny.core._


class FixedPriceMarket extends IMarket{ 
	var filename : String = "defaultPriceList.txt"
	var priceList : PriceList = new PriceList(filename) 

 	def addAirTime(plane : String, millis : Long) = ()
	def cycle(name : String) = ()
	 
  
	override def getPrice(plane : String) : Double = {
	  // todo
	  0.0
	}
	
	
 
	def setConfiguration(pathToFile : String) = {
	  // todo
	  false
	} 
	
 
 
	
}
