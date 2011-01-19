package de.immaterialien.sturmonanny.market.fixed
import net.lag.configgy
import _root_.de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema


class PriceList(file:String) extends ConfigurationSchema(file) {
	object divisor extends Field(1){doc="prices can be given only in full numbers, use this to scale them down for precision"}
	object planes extends Table(0)  {
	doc = """ 
Beispiel:
	<planes>
      # F6F-3 mit default loadout (oder einem nicht anderweitig aufgeführten loadout) gibt 100 pro Minute:
      F6F-3 = -100   

      # das gleiche Flugzeug mit loadout 6xhvarap kostet 50 pro Minute:
      F6F-3_6xhvarap = 50A

      # Achtung: in einigen Loadout-Bezeichnungen wird "*" oder "+" verwendet, 
      # dies kann für die Bezeichnung hier leider nicht verwendet werden und ist hier durch "x" (für "*") 
      # zu ersetzen bzw. zu entfernen (für "+") 
      # Aus 1*ParaFlare+6*10kg würde also 1xParaFlare6x10kg, was dem Bezeichnungsmuster der original-1C-Loadouts entspricht 
			S-328_1xParaFlare6x10kg = 20
	</planes>
""" 
	}
	object red extends Table(0)  {
		doc = "like planes but only applies to red"
	}
  object blue extends Table(0)  {
		doc = "like planes but only applies to blue"
	}
 }
object PriceList {
  val default = new PriceList("")
}