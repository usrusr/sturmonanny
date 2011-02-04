package de.immaterialien.sturmonanny.market.fixed

import org.junit._
 import _root_.de.immaterialien.sturmonanny.core.IMarket._
 
class FixedPriceTest {
	@Test
	def test {
		val m = new FixedPriceMarket
		m.setConfiguration("planes.conf")

		val eqs = new java.io.File("C:/Test") == new java.io.File("C:\\test") 
		println("e-> "+eqs)
		var p = m.tryPrice(Loadout("Yak-7B(PF)", None), 1)
		println("p-> "+p)
		println("m-> "+m.priceList)
	}
} 
object FixedPriceTest {
	def main(args:Array[String]){
		
		println("map:"+(Map((1->10), 2->20) ++ Map((1->11), 3->23))) 
		
		
		new FixedPriceTest().test
	}
}