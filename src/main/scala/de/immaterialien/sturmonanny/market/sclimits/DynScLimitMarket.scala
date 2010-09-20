package de.immaterialien.sturmonanny.market.sclimits
import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._
import IMarket._
import java.io._
import scala.collection._
class DynScLimitMarket extends IMarket with Log{ import DynScLimitMarket._
	val priceFactor = 10D
	val supplyFactor = 2D // minutes for one SC plane

	val sides = Map(new SideMarket(1).pair, new SideMarket(2).pair)
  override def addAirTime(load : Loadout, millis : Long, side:Int) = sides(side).addAirTime(load, millis)
  protected def tryPrice(loadout:Loadout, side:Int) = sides(side).tryPrice(loadout)
  
  /**
	 * return true for a successful configuration update 
	 */  
	override def setConfiguration(pathToFile : String) : Boolean = true
	override def setServerContext(server : Server) : Unit = true
	
	
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
	println("add "+side+" plane "+i+" of "+which)					
						planes.put(which, planes.get(which).map(_ + i).getOrElse(i))
					}
					case x => 
	println(" ignoring '"+x+"'" )				
				}
			}
			source.close
	}
	
  
	class SideMarket(s:Int) {
		def pair=(s, this)
	
		var pl = Map[String, Double]()
		var supply = Map[String, Double]()
		private var use = Map[String, Variable[Double]]()
		var totalsupply : Double = 0D
		
		
		def tryPrice(loadout:Loadout) : Option[Double] = {
	log debug "trying price for "+loadout+" in side "+s
			pl.get(loadout.plane )
			
	  }
		
			
		/**
		 * @param plane
		 * @param millis
		 */
		def addAirTime(load : Loadout, millis : Long)={
	println("using "+load+ " from "+use )		
			for(
					plane<-use.get(load.plane)
			){
				val newv = plane.v - (millis.toDouble/60000) 
	println(".. new value "+plane.v)		
				plane.v = newv
				if(newv<0D) recalculate()
			}
		}

		def cycle(newSupply: Map[String, Double]){
println("cycling "+s+" with "+newSupply)			
			supply=newSupply
			totalsupply = supply.values.foldLeft(0D)(_+_)
			
			
			// create a copy of supply with 0-Variables
			use = supply.map(y=>(y._1, new Variable(0D)))
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
	println("adding "+supp+" to "+plane+" -> "+(variable.v + supp))				
					variable.v = variable.v + supp
				}
			} 
			
			pl = { 
				val supplyCount:Double = totalsupply
				val sum = use.map(_._2.v).foldLeft(0D)(_ + _)
				val relativeSum = sum/supplyCount
				
				val total = supply.size
	
				val avg = sum / total
	println("side "+s)				
	println("total: "+total+" avg:"+avg+" sum:"+sum +"  _> relativesum: "+relativeSum)				
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
			
	println("recalculated: "+pl)		
			}
	
	}


}
object DynScLimitMarket {
	val army = """\s*\[\s*PlanesArmy(\d)\s*\]\s*""".r
	val ini = """\s*\[\s*([^\]]*?)\s*\]\s*""".r
	val planeCount = """\s*([^\s=]+)\s*=\s*(\d+)(?:\s*,.*)?""".r 	
}
private class Variable[T](var v:T)