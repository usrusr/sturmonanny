package de.immaterialien.sturmonanny.market.fixed
import org.junit.Test
import net.lag.configgy
import de.immaterialien.sturmonanny.util.ConfiggyFile



class PriceListTest {
	@Test 
	def testFile() : Unit ={
		val p = new PriceList("E:/eclipseworkspace/sturmonanny/src/test/scala/de/immaterialien/sturmonanny/market/fixed/test.conf")
  
		println("price list is \n"+ p)
  
		println("Spit: "+ p.prices.planes("Spit"))
		println("Porsche: "+ p.prices.planes("Porsche"))
		p.prices.planes("Spit") = 3
		println("changed Spit: "+ p.prices.planes("Spit"))
		p.prices.planes("P51") = 2
		p.aliases.planes("P51") = "Mustang"
		p.aliases.pilots("usrusr") = "Ulf"
//		p.prices.planes("String") = "geht nicht"
		
  
  
  		println("price list is \n"+ p)
		()
	}
}
class PriceList(file:String) extends ConfiggyFile(file){
	object prices extends Group{ 
	  object host extends Field( "127.0.0.1")
	  object il2port extends Field(2001)   	  
    
	  object planes extends  Table(0)
	  object consoleport extends Field(2011)
	}
	object aliases extends Group{
	  object planes extends  Table("noPlane")
	  object pilots extends  Table("noPlane")
   }
	 
}