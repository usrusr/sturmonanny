package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny.core._


class FixedPriceMarket extends IMarket{ 
	var filename : String = "defaultPriceList.txt"
	var priceList : PriceList = new PriceList(filename) 

 	def addAirTime(plane : String, millis : Long) = ()
	def cycle(name : String) = ()
	 
  
	override def getPrice(plane : String) : Double = {
	  priceList.planes(plane)
	}
	
	
 
	def setConfiguration(pathToFile : String) = {
	  if(pathToFile==filename) true
	  else {
		  val newList = new PriceList(pathToFile)
		  if(newList.initialized) {
			  priceList = newList
			  filename = pathToFile
			  true
		  } else false
	  }
	} 
}
