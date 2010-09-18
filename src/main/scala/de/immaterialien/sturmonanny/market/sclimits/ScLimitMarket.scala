package de.immaterialien.sturmonanny.market.sclimits
import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._
import IMarket._
import java.io._
import scala.collection._
class ScLimitMarket extends IMarket with Log{ import ScLimitMarket._
	
	var pl = Map[Int, Map[String, Double]]()
	protected def tryPrice(loadout:Loadout, side:Int) : Option[Double] = {
log debug "trying price for "+loadout+" in side "+side
		pl.get(side).flatMap(_ get(loadout.plane ))
		
  }
	
		
	/**
	 * @param plane
	 * @param millis
	 */
	override def addAirTime(plane : Loadout, millis : Long, side:Int)=()
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
		
		var planes = new mutable.HashMap[String, Int]()  
		var side : Option[Int] = None 
		var ret = Map[Int, Map[String, Double]]()
		def processSide() = {
println("processing "+side)			
			for(s<-side){
println(s+" planes raw: "+planes)			
				
				var prices = Map[String, Double]()
				val sum = planes.map(_ _2).foldLeft(0)(_ + _)
				val total = planes.size
				val avg = sum.toDouble / total.toDouble
				
				
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
				prices = planes.mapValues(count=>{
					if(count==avg)0D else {
						val sign = if(count > avg) -1D else 1D
						val factor = 10D
					
						sign*factor*(total.toDouble / count.toDouble)
					}
					
				})
				println(s+" planes proc: "+prices)		
				ret = ret + ((s, prices))
			}
			planes.clear()
		}
		for(line <- source.getLines){
			(side, line) match {
				case (_ , army(is))=>{
					processSide
					side = try{Some(is.toInt)}
				}
				case (_ , ini(_))=>{
					processSide
					side = None
				}
				case (Some(_), planeCount(which, is)) => {
					val i = is.toInt
println("add "+side+" plane "+i+" of "+which)					
					planes.put(which, planes.get(which).map(_ + i).getOrElse(i))
				}
				case x => 
println(" ignoring '"+x+"'" )				
			}
			log debug "new prices "+ret
			pl = ret
		}
		source.close
	}

}
object ScLimitMarket {
	val army = """\s*\[\s*PlanesArmy(\d)\s*\]\s*""".r
	val ini = """\s*\[\s*([^\]]*?)\s*\]\s*""".r
	val planeCount = """\s*([^\s=]+)\s*=\s*(\d+)(?:\s*,.*)?""".r 	
}