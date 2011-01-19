package de.immaterialien.sturmonanny.market.fixed

import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._


class FixedPriceMarket extends IMarket with Logging{ 
	var filename : Option[String] = None 
	var priceList : Option[PriceList] = None  
	var server : Option[Server] = None 

 	def addAirTime(plane : IMarket.Loadout, millis : Long, side:Int) = ()
	def cycle(name : java.io.File) = ()
	
	
  
	override def tryPrice(loadout : IMarket.Loadout, side:Int) : Option[Double] = {
	  //val res = priceList.map(_.planes(plane)) map (_ toDouble)
		
		
		val name = loadout.toString
			.replace("*", "x")
			.replace("+", "")
			.replace(" ", "")
			
		for(list <- priceList) {
			val maps = side match {
				case 1 => List(list.red, list.planes)
				case 2 => List(list.blue, list.planes)
				case _ => List(list.red, list.blue, list.planes)
			}
			
			for(m<-maps){
				val r = m.get(name)
				if(r.isDefined) return Some(
						r.get.toDouble/list.divisor.apply.toDouble)
			}
		}
		None
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
			  		list<-List(l.planes, l.red, l.blue);
			  		(plane, _) <- list.map){
			    val planes : Planes = srv.planes 
debug("creating "+plane+" in market -> "+planes.items )		    
			    planes.create(plane)
			  }
		  true
	  }catch{
	  	case e => {
warn("market configuration failed ", e)	  		
		  	false 
		  }
	  }
	} 
}
