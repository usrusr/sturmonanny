package de.immaterialien.sturmonanny.market.fixed
import org.junit.Test
import org.junit.Assert._
import net.lag.configgy
import de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema



class PriceListTest {
	@Test 
	def testFile() : Unit ={
		val p = new MyList("E:/eclipseworkspace/sturmonanny/src/test/scala/de/immaterialien/sturmonanny/market/fixed/test.conf")

		println("price list is \n"+ p )
		println("host is is "+ p.prices.host )
  		assertTrue("host should be example.com "+p.prices.host , p.prices.host.apply == "example.com")
  
		println("Spit: "+ p.prices.planes("Spit"))
		println("Porsche: "+ p.prices.planes("Porsche"))
		p.prices.planes("Spit") = 3
		println("changed Spit: "+ p.prices.planes("Spit"))
		p.prices.planes("P51") = 2
		p.aliases.planes("P51") = "Mustang"
		p.aliases.pilots("usrusr") = "Ulf"
//		p.prices.planes("String") = "geht nicht"
		 
  		p.aliases.group.subgroup.test() = "na sowas!"
  		p.prices.pricegroup.subgroup.test() = "so ein pries"
  
  		println("price list is \n"+ p) 
		()
	} 
} 
class MyList(file:String) extends ConfigurationSchema(file){
	object prices extends Group{ 
	  object d1 extends Documentation(""" host and port:
host and il2port are provided as examples
""")
	  object host extends Field( "127.0.0.1")
	  object il2port extends Field(2001)   	  

   	  object d2 extends Documentation(""" planes:
the planes table maps plane name strings to prices, which have to be positive or negative whole numbers 
""")

	  object planes extends  Table(0)
	  object consoleport extends Field(2011)
      object pricegroup extends Group{
   	    object subgroup extends Group{
   	    	object test extends Field("possible? no, does not initialize (make group a member and we are set!)")
	    }
	  } 
	}
	object aliases extends Group{
	  object planes extends  Table("noPlane")
   	  object group extends Group{
   	    object subgroup extends Group{
   	    	object test extends Field("possible? no, does not initialize (make group a member and we are set!)")
	    }
	  } 
	  object pilots extends  Table("noPlane")
   }
	 
}