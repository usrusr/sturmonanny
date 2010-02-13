package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny.core._
import de.immaterialien.sturmonanny.util._


class FixedPriceMarket extends IMarket with Logging{ 
	var filename : Option[String] = None
	var priceList : Option[PriceList] = None  
	var server : Option[Server] = None

 	def addAirTime(plane : String, millis : Long) = ()
	def cycle(name : String) = ()
	 
  
	override def getPrice(plane : String) : Double = {
	  val res = priceList.map(_.planes(plane)) map (_ toDouble)
	  res getOrElse 0
	}
	override def setServerContext(srv:Server){
debug("setting server context: "+srv)		    
	  server = Some(srv)
    }

	def setConfiguration(pathToFile : String) = {
	  if(Some(pathToFile)==filename) true
	  else {
		  val newList = new PriceList(pathToFile)
		  if(newList.initialized) {
debug("initializing market")		    
			  priceList = Some(newList)
			  filename = Some(pathToFile)
			  // make sure we know all planes, for queries into the plane domain
			  for(	srv <- server; 
			  		l<-priceList; 
			  		(plane, _) <- l.planes.map){
			    val planes : Planes = srv.planes
debug("creating "+plane+" in market -> "+planes.items )		    
			    planes.create(plane)
			  }
     
			  true
		  } else false
	  }
	} 
}
