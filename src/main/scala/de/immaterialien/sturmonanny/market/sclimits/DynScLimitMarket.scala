package de.immaterialien.sturmonanny.market.sclimits
import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._
import IMarket._
import java.io._
import scala.collection._
class DynScLimitMarket extends IMarket with Log{ import DynScLimitMarket._
	
	private var pl = Map[Int, Map[String, Double]]()
	private var supply = Map[Int, Map[String, Double]]()
	private var use = Map[Int, Map[String, Variable[Double]]]()
	
	protected def tryPrice(loadout:Loadout, side:Int) : Option[Double] = {
log debug "trying price for "+loadout+" in side "+side
		pl.get(side).flatMap(_ get(loadout.plane ))
		
  }
	
		
	/**
	 * @param plane
	 * @param millis
	 */
	override def addAirTime(load : Loadout, millis : Long, side:Int)={
println("using "+load)		
		for(
				us<-use.get(side);
				plane<-us.get(load.plane)
		){
			val newv = plane.v - (millis.toDouble/60000) 
println(".. new value "+plane.v)		
			plane.v = newv
			if(newv<0D) recalculate()
		}
	}
	/**
	 * return true for a successful configuration update 
	 */  
	override def setConfiguration(pathToFile : String) : Boolean = true
	
	override def setServerContext(server : Server) : Unit = true
	
	def recalculate() {
		for(
			(side, map)<-use;
			(plane, variable)<-map
		){
			for(
				us<-supply.get(side);
				supp<-us.get(plane)					
			) {
println("adding "+supp+" to "+plane)				
				variable.v = variable.v + supp
			}
		} 
		
		pl = for((side, planes)<-use) yield { 
			val sum = planes.map(_._2.v).foldLeft(0D)(_ + _)
			val sideRet = for((plane, variable)<-planes) yield {
				val count = variable.v
				var prices = Map[String, Double]()
				val total = planes.size
				val avg = sum / total
				
				
				/* example calculation:
				 * 
				 * 10 spit
				 * 30 hurri
				 * 
				 * avg 20
				 * total 40
				 * 
				 * spit: 10 less than average
				 * hurry: 10 more than average
				 * 
				 * but we need 3 times as many hurri in the air:
				 * 
				 *                      
				 * spit:  40/10 = 12/3   =>  3  => 120
				 * hurry: 40/30 = 4/3    => -1  => -40
				 * 
				 * *3:
				 * spit:  =>  3
				 * hurry: => -1
				 *
				 *  since the exact factor is irrelevant we use 
				 */

					val r = if(count==avg)0D else {
						val sign = if(count > avg) -1D else 1D
						val factor = 10D
					
						sign*factor*(total.toDouble / count.toDouble)
					}
					(plane,r)
				}
//				println(s+" planes proc: "+prices)		
//				ret = ret + ((s, prices))
				(side, sideRet)
			}
		}
	
	
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
		supply = Map()
		
		for(line <- source.getLines){
			(side, line) match {
				case (_ , army(is))=>{
					for(s<-side) {
						var ps = Map[String, Double]()
						ps = ps ++ planes
						supply = supply + ((s, ps))
						planes.clear()
					}
					side = try{Some(is.toInt)}
				}
				case (_ , ini(_))=>{
					for(s<-side) {
						var ps = Map[String, Double]()
						ps = ps ++ planes
						supply = supply + ((s, ps))
						planes.clear()
					}
					side = None
				}
				case (Some(_), planeCount(which, is)) => {
					val i = is.toInt.toDouble
println("add "+side+" plane "+i+" of "+which)					
					planes.put(which, planes.get(which).map(_ + i).getOrElse(i))
				}
				case x => 
println(" ignoring '"+x+"'" )				
			}
			log debug "new supply "+supply
		}
		source.close
		
		// create a copy of supply with 0-Variables
		use = supply.map(x=>(x._1, x._2.map(y=>(y._1, new Variable(0D)))))
		recalculate()
	}

}
object DynScLimitMarket {
	val army = """\s*\[\s*PlanesArmy(\d)\s*\]\s*""".r
	val ini = """\s*\[\s*([^\]]*?)\s*\]\s*""".r
	val planeCount = """\s*([^\s=]+)\s*=\s*(\d+)(?:\s*,.*)?""".r 	
}
private class Variable[T](var v:T)