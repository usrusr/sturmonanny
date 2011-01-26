package de.immaterialien.sturmonanny.market.fixed
import java.io._
import scala.collection.mutable

/**
 * a quick and dirty reimplementation of the interface used by PriceList, with plain regex reading instead of configgy
 * @author ulf
 *
 */
class RegexPriceList (file:String) { import RegexPriceList._
	private object collector {
		var divisor = 1
		val planes = new mutable.HashMap[String, Double]()
		val red = new mutable.HashMap[String, Double]()
		val blue = new mutable.HashMap[String, Double]()
		private[RegexPriceList] def init {
			val reader = new BufferedReader(new FileReader(file))
			try{
				var line = reader.readLine
				var rec : Option[mutable.HashMap[String, Double]] = None
				while(line!=null){
					if(rec.isEmpty) line match {
						case price("divisor", num) => collector.divisor = num.toInt
						case planesBegin() => rec = Some(collector.planes)
						case redBegin() => rec = Some(collector.red)
						case blueBegin() => rec = Some(collector.blue)
						case ignore() => 
						case _ => println("failed to parse price list entry '"+line+"' in "+rec)							
					}else line match {
						case price(name, num) => rec.get += (name -> num.toInt)
						case planesEnd() => rec = None
						case redEnd() => rec = None
						case blueEnd() => rec = None
						case ignore() => 
						case _ => println("failed to parse price list entry '"+line+"' in "+rec)							
					}
					line = reader.readLine
				}
			}catch{case x => x.printStackTrace }finally{try{reader.close}}
		}
	}
	
	collector.init
	class MapIndirection(private val m:mutable.HashMap[String, Double]){
		def get(n:String) = m.get(n)
		def map = m
	}
	val planes = new MapIndirection(collector.planes)
	val red = new MapIndirection(collector.red)
	val blue = new MapIndirection(collector.blue)
//		def get(n:String) = collector.planes.get(n)
//		def map = collector.planes
//	}
//	object red {
//		def get(n:String) = collector.red.get(n)
//		def map = collector.red
//	}
//	object blue {
//		def get(n:String) = collector.blue.get(n)
//		def map = collector.blue
//	}
	object divisor {
		def apply = collector.divisor
	}
	
}
object RegexPriceList {
//	val comment = """^\s*#.*$""".r
	val planesBegin = """^\s*<planes>\s*$""".r
	val planesEnd = """^\s*</planes>\s*$""".r
	val blueBegin = """^\s*<blue>\s*$""".r
	val blueEnd = """^\s*</blue>\s*$""".r
	val redBegin = """^\s*<red>\s*$""".r
	val redEnd = """^\s*</red>\s*$""".r
	val price = """^\s*(\S+)\s*=\s*(-?\d+)\s*$""".r	
	val ignore = """^\s*(?:</?PriceList>|#.*)?\s*$""".r
}