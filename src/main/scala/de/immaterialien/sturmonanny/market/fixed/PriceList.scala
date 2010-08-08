package de.immaterialien.sturmonanny.market.fixed
import net.lag.configgy
import _root_.de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema


class PriceList(file:String) extends ConfigurationSchema(file) {
	object planes extends Table(0)  {
	doc = """ Beispiel:
	<planes>
# F6F-3 mit default loadout (oder einem nicht anderweitig aufgeführten loadout) gibt 100 pro Minute:
F6F-3 = -100   

# das gleiche Flugzeug mit loadout 6xhvarap kostet 50 pro Minute:
F6F-3:6xhvarap = 50
 
	</planes>
"""
	}
 }
object PriceList {
  val default = new PriceList("")
}