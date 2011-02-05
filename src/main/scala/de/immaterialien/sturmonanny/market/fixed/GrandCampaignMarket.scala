package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._
import scala.collection._


object GrandCampaignMarket {
	val misNameExtractor = """^(.+)(\d{9})\.mis$""".r
}

class GrandCampaignMarket extends IMarket with Logging{  
import GrandCampaignMarket._
	var filename : Option[String] = None 
	var priceList : Map[Int, Map[String,Double]] = Map()  
	var server : Option[Server] = None 
	var planesInMis : Map[Int,Set[String]] = Map() 
  var misFile : java.io.File = _
 	def addAirTime(plane : IMarket.Loadout, millis : Long, side:Int) = ()
 	var priceListParser : ParserPriceList = _
 	var lastUpdate = 0L
	
//	var timeTable : immutable.SortedMap[(String, String), Map[Int, Map[String,Double]]] = null
  
	def cycle(name : java.io.File) = {
		misFile = name
		planesInMis = AvailablePlanesIdentifier.cycle(misFile)
		val updateRes = priceListParser.checkUpdate
		for(update <- updateRes){
//			timeTable=update
			updateFromTimeTable
		}
		
//		for(
//				set<-planesInMis.values;
//				plane<-set
//				) {
//debug("creating "+plane+" in market ")		    
//			    planes.create(plane)
//
//		}
//		def dateFloor(date:String) = date+("0"*(9-math.min(date.length, 9))) 
		
	}
 	def updateFromTimeTable{
 		lastUpdate = System.currentTimeMillis
 		val (campa, misDate) = misFile.getName match {
			case misNameExtractor(str,date) => (str,date+("9"*(9-date.length)))
			case _ => {
				error("failed to identify GrandCampaignPricing name for "+misFile+" , using 'default' entry")
				("default", "000000000")
			}
		}
 		val misLong = misDate.toLong
		var result : Map[Int, Map[String,Double]] = Map()  
		for(((entryCampa,dateLimit) , thisMap)<-priceListParser.timeTable){
//			val dateLimit = dateFloor(sinceMis._2)
//			val entryCampa=sinceMis._1
//println("dateLimit:"+dateLimit+" misdate:"+misDate)			
			if(entryCampa == campa && dateLimit.toLong <= misLong){
				for((side,prices)<-thisMap){
					result += (side -> (result.get(side).getOrElse(Map[String,Double]()) ++ prices ))
				}
			}
		} 		
		priceList = result
 	}


	override def tryPrice(loadout : IMarket.Loadout, side:Int) : Option[Double] = {
		if(System.currentTimeMillis - lastUpdate > 10000){
			for(update <- priceListParser.checkUpdate){
//				timeTable=update
				updateFromTimeTable
			}			
		}
		val existing = 
		planesInMis.get(side).map{mySide=>
			val contains=mySide.contains(loadout.plane)
//println(side+" contains "+contains+" "+loadout.plane+" in "+mySide)			
			contains
		}.getOrElse{
			! planesInMis.values.projection.filter{someSide=>
				someSide.contains(loadout.plane)
			}.isEmpty
		}
		if( ! existing) return None
		
		val name = loadout.toString
			.replace(" ", "")
//			.replace("*", "x")
//			.replace("+", "")
//			.replace("(", "_")
//			.replace(")", "_")
//			.replace(".", "_")
			
		for(list <- priceList) {
			val maps = (side match {
				case 1 => List(1,0)
				case 2 => List(2,0)
				case _ => List(1,2,0)
			}).map{index=>
				priceList.get(index) getOrElse Map[String,Double]()
			}
			
			for(m<-maps){
				val r = m.get(name) 
				if(r.isDefined) return Some(
						r.get.toDouble)
			}
		}
		None
	}
	override def setServerContext(srv:Server){
debug("setting server context: "+srv)		    
	  server = Some(srv)
    }
 
	def setConfiguration(pathToFile : String) = {
	  val ret = if(Some(pathToFile)==filename) true
	  else try {
		  priceListParser = new ParserPriceList(new java.io.File(pathToFile))
//		  timeTable = priceListParser.timeTable
		  true
	  }catch{
	  	case e => {
warn("market configuration failed ", e)	  		 
		  	false  
		  } 
	  } 
	  
//	  timeTable = priceListParser.checkUpdate.getOrElse(timeTable)
	  
	  ret
	} 
}
