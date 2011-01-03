package de.immaterialien.sturmonanny.market.sclimits

import java.io._
import _root_.de.immaterialien.sturmonanny.core._
import _root_.de.immaterialien.sturmonanny.util._
import IMarket._

object RunDynScLimitMarket {
	def main(args: Array[String]) {
		//testDyn
		//testMem
		testDynIasi
	}
	
	def testDynIasi {
		val m = new DynScLimitMarket
//		var f = new File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194308140.mis")
		var f = new File("src/test/resources/de/immaterialien/sturmonanny/market/sclimits/iasi.mis")
		m.cycle(f)
		
		println("init: "+m.sides)
		
		var name1 = "Hs-129B-2"
		name1 = "Bf-109G-6_Late"
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)		
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)		
		
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)		
	}

	
	def testDyn {
		val m = new DynScLimitMarket
		var f = new File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194308140.mis")
		//var f = new File("src\test\resources\de\immaterialien\sturmonanny\market\sclimits\iasi.mis")
		m.cycle(f)
		var name1 = "F6F-3"
		name1 = "P-38J"
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)		
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)		
		
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)
		m.addAirTime(Loadout(name1, None),60000*5, 1)		
	}
	def testMem {
		val tf = java.io.File.createTempFile("sc_market_mem", "planecount")
		
		val m = Map(
				2 -> Map(
						"flugi 1" -> 17D,
						"flugi 2" -> 18D
				),
				1 -> Map(
						"flugi 1" -> 19D,
						"flugi 3" -> 18D
				)
		)
		
		DynScLimitMemory.store(m, tf)
		val nm = DynScLimitMemory.load(tf)
		
println("old "+m)		
println("new "+nm)		
println("equals?"+(m==nm))		
	}
}