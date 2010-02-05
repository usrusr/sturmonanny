package de.immaterialien.sturmonanny.market.fixed
import org.junit.Test

class PriceListTest {
	@Test 
	def testFile() : Unit ={
		val p = new PriceList("default.conf")
  
		println("price list is \n"+ p)
  
		()
	}
}
