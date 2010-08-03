package de.immaterialien.sturmonanny.market.fixed

import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._


class FixedPriceMarket extends IMarket with Logging{ 
	var filename : Option[String] = None 
	var priceList : Option[PriceList] = None  
	var server : Option[Server] = None 

 	def addAirTime(plane : IMarket.Loadout, millis : Long) = ()
	def cycle(name : String) = ()
	
	
  
	override def tryPrice(loadout : IMarket.Loadout) : Option[Double] = {
	  //val res = priceList.map(_.planes(plane)) map (_ toDouble)
		
		val res = for(list <- priceList) yield list.planes(loadout.toString)
		
		
	  res map (_ toDouble)
	}
	override def setServerContext(srv:Server){
debug("setting server context: "+srv)		    
	  server = Some(srv)
    }
 
	def setConfiguration(pathToFile : String) = {
	  if(Some(pathToFile)==filename) true
	  else try {
		  val newList = new PriceList(pathToFile)
		  
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
	  }catch{case _ => false }
	} 
}
