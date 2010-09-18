package de.immaterialien.sturmonanny.market.sclimits

import java.io._

object RunScLimitMarket {
	def main(args: Array[String]) {
		val m = new ScLimitMarket
		var f = new File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194308140.mis") 
		m.cycle(f)
	}
}