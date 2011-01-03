package de.immaterialien.sturmonanny.market.sclimits
import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._
import IMarket._
import java.io._
import scala.collection.{mutable, immutable}
class DynScLimitMarket extends IMarket with Log{ import DynScLimitMarket._
	val priceFactor = 10D
	val supplyFactor = 2D // minutes for one SC plane
	val leftoverWeight = 1D // 1D: inital prices after cycle will be 50:50 derived from leftover and new supply, bigger weight: more leftover,
	val memoryName = "DynScLimitMarket.leftovers.txt"

	var server : Option[Server]=None 
	override def setServerContext(srv:Server){
log.debug("setting server context: "+srv)		    
	  server = Some(srv)
  }
		
	val sides = Map(new SideMarket(1).pair, new SideMarket(2).pair)
  override def addAirTime(load : Loadout, millis : Long, side:Int) = sides(side).addAirTime(load, millis)
  protected def tryPrice(loadout:Loadout, side:Int) = {
		if(side>0) sides(side).tryPrice(loadout)
		else {
			val s1 = sides(1).tryPrice(loadout)
			if(s1 isDefined) s1 
			else sides(2).tryPrice(loadout)
		}
	}

  // holds the remaining capacities of the last cycle, or None for first access
 	var memory : Option[Map[Int, Map[String, Double]]] = None

  /**
	 * return true for a successful configuration update 
	 */  
	override def setConfiguration(pathToFile : String) : Boolean = true

	
	
			/**
		 * notify market of a new map
		 * 
		 * @param name
		 */
		override def cycle(name : File):Unit = if(name!=null){
			val misName = name.getAbsolutePath
			val iniName = misName.substring(0, misName.length-4)+".ini"
	log debug "cycling to "+iniName	
			val source = scala.io.Source.fromFile(iniName)
			
			if(memory.isDefined){
				// update with current state, then store
				memory = Some{
					var mem : Map[Int, Map[String, Double]]= sides.map{kv=>
						(kv._1, kv._2.remaining)
					}
					val toFile = new File(name.getParent, memoryName)
					val bakFile = new File(name.getParent, memoryName+".bak")
					try {
						if(toFile.exists){
							if(bakFile.exists && bakFile.delete) toFile.renameTo(bakFile) 
						}
					}catch{
						case x=> log.warning("failed to backup previous leftover plane minute contingents, overwriting "+toFile.getAbsolutePath,x)
					}
					try {
						DynScLimitMemory.store(mem, toFile)
					}catch{
						case x=> log.warning("failed to store leftover plane minute contingents in "+toFile.getAbsolutePath+", keeping capacities only in RAM",x)
					}
					mem
				}
			} else {
				// uninitialized memory
				val fromFile = new File(name.getParent, memoryName)
				memory = try{
					Some(DynScLimitMemory.load(fromFile))
				}catch{
					case x=> {
						log.warning("failed to load leftover minute contingents from "+fromFile.getAbsolutePath+", using empty contingengts",x)
						Some(sides.map{kv=> (kv._1, kv._2.remaining)})
					}
				}
			}

			var planes = new mutable.HashMap[String, Double]()  
			var side : Option[Int] = None 
			
			for(line <- source.getLines){
				(side, line) match {
					case (_ , army(is))=>{
						for(s<-side) {
							var ps = Map[String, Double]()
							ps = ps ++ planes 
							sides(s).cycle(ps)
							planes.clear()
						}
						side = try{Some(is.toInt)}
					}
					case (_ , ini(_))=>{
						for(s<-side) {
							var ps = Map[String, Double]()
							ps = ps ++ planes
							sides(s).cycle(ps)
							planes.clear()
						}
						side = None
					}
					case (Some(_), planeCount(which, is)) => {
						val i = is.toInt.toDouble * supplyFactor
	log.debug("add "+side+" plane "+i+" of "+which)					
						planes.put(which, planes.get(which).map(_ + i).getOrElse(i))
					}
					case x => 
	log.debug(" ignoring '"+x+"'" )				
				}
			}
			source.close
			
		  for(	srv <- server){
	  		val planes : Planes = srv.planes 
		  	planes.clear
		  	for(	
		  		side<-sides.values; 
		  		plane<-side.pl.keys
		  	){
log.debug("creating "+plane+" in market -> "+planes.items )		    
		    	planes.create(plane)
		  	}
		  }			
	}
	
  
	class SideMarket(s:Int) {
		def pair=(s, this)
		override def toString = "\ns:\n  "+pl.mkString("\n  ")
		var pl = Map[String, Double]()
		var supply = Map[String, Double]()
		private var use = Map[String, Variable[Double]]()
		
		// sum of supply
		var totalsupply : Double = 0D
		
		def remaining : Map[String, Double] = {
			use.map{kv=>
				(kv._1, kv._2.v)
			}
		}
		def tryPrice(loadout:Loadout) : Option[Double] = {
	log debug "trying price for "+loadout+" in side "+s
			pl.get(loadout.plane )
			
	  }

			
		/**
		 * @param plane
		 * @param millis
		 */
		def addAirTime(load : Loadout, millis : Long)={
	log.debug("using "+load+ " from "+use )		
			for(
					plane<-use.get(load.plane)
			){
				val newv = plane.v - (millis.toDouble/60000) 
	log.debug(".. new value "+plane.v)		
				plane.v = newv
				if(newv<0D) recalculate()
			}
		}

		def cycle(newSupply: Map[String, Double]){
log.debug("cycling "+s+" with "+newSupply)			
			supply=newSupply
			totalsupply = supply.values.foldLeft(0D)(_+_)

			val leftoverMap : Map[String, Double]= memory.flatMap(_ get s) getOrElse Map()
			// include leftover types that are not available in the current mission in the scale calculation
			val totalleftover = leftoverMap.values.foldLeft(0D)( _ + _ )
			val leftoverScale = math.min(
					leftoverWeight, 
					if(totalleftover > 0D) 
						leftoverWeight * totalsupply / totalleftover
					else 0D  
			)
			// create a copy of supply with Variables containing the scaled leftovers
			use = supply.map{y=>
				val scaledLeftover = leftoverScale * leftoverMap.get(y._1).getOrElse(0D)
				(y._1, new Variable(scaledLeftover)) 
			}
			


			recalculate()
			()
		}	
		def recalculate() {
			for(
				(plane, variable)<-use
			){
				for(
					supp<-supply.get(plane)					
				) {
	log.debug("adding "+supp+" to "+plane+" -> "+(variable.v + supp))				
					variable.v = variable.v + supp
				}
			} 
			
			pl = { 
				val supplyCount:Double = totalsupply
				val sum = use.map(_._2.v).foldLeft(0D)(_ + _)
				val relativeSum = sum/supplyCount
				
				val total = supply.size
	
				val avg = sum / total
	log.debug("side "+s)				
	log.debug("total: "+total+" avg:"+avg+" sum:"+sum +"  _> relativesum: "+relativeSum)				
				val sideRet = for((plane, variable)<-use) yield {
					val count = variable.v
					/* example calculation:
					 * 
											5 spit
											10 p40
											15 hurri
											
											avg 10
											total 40
											
											spit: 5 less than average
											hurry: 5 more than average
											
											but we need 3 times as many hurri in the air, so h would b1 -1, s would be +3
											
											a/c-1*x:
											
											p40       
											10/10 -> 1 -1 -> 0  *-10 => 0
											               
											spit:  
											10/5 -> 2 -1 -> 1 
											
											hurry: 
											10/15 = 0,66 - 1 -> -0,33
											
					 */
	
					val r = if(count==0D) 0D else {
						priceFactor * (avg/count - 1)
					}
						(plane,r)
					}
					sideRet
				}
			
	log.debug("recalculated: "+pl)		
			}
	}
}
object DynScLimitMarket {
	val army = """\s*\[\s*PlanesArmy(\d)\s*\]\s*""".r
	val ini = """\s*\[\s*([^\]]*?)\s*\]\s*""".r
	val planeCount = """\s*([^\s=]+)\s*=\s*(\d+)(?:\s*,.*)?""".r 	
}
private class Variable[T](var v:T)