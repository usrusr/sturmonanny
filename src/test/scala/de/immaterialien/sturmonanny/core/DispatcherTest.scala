package de.immaterialien.sturmonanny.core

import org.junit.Test


class DispatcherTest {
	@Test 
	def testRegex() : Unit ={
		val d = new Dispatcher() 
		d ! """Chat: Mad: \ttest msg T s\n"""
		d ! "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
		d ! "\\"+"""u00201      Mad              1       0      (2)Blue     < +         Bf-109G-2\n"""
//Thread sleep 2000  
		d ! "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
		d ! "\\"+"""u00201      Mad              1       0      (2)Blue     red-2       Porsche\n"""
		d ! "\\"+"""u00201      Sturm0jetty      1999    -29000 (1)Red      \n"""
//Thread sleep 2000  
		d ! "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
		d ! "\\"+"""u00201      Mad              1       0      (2)Blue     red-2       IL-2\n"""
	}
}
 