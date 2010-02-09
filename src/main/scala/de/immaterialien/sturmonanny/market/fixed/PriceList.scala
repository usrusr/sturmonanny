package de.immaterialien.sturmonanny.market.fixed
import net.lag.configgy
import de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema


class PriceList(file:String) extends ConfigurationSchema(file){
	object comment extends Documentation(""" Beispiel:
<planes>
  Ju-87D-5 = -10
  Bf-109G-2 = 15 
</planes>
""")
	object planes extends Table(0)
}
object PriceList {
  val default = new PriceList("")
}