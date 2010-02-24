package de.immaterialien.sturmonanny.market.fixed
import net.lag.configgy
import de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema


class PriceList(file:String) extends ConfigurationSchema(file) {
	object planes extends Table(0)  {
	doc = """ Beispiel:
	<planes>
	  La-5FN = 50
	  La-5F = 40
	  Pe-2series110 = -100 
	  Il-2_3 = -80
	  Il-2M_Late = -70


	Bf-109G-2=30
	Bf-109G-6_Late=40
	Fw-190F-8=60
	He-111H-6=-100
	IAR81c=-80
	Ju-87D-5=-60
	Ju-87G-1=-70
	Ju-88A-4=-70

	</planes>
"""
	}
 }
object PriceList {
  val default = new PriceList("")
}