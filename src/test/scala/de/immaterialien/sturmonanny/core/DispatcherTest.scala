package de.immaterialien.sturmonanny.core

import org.junit.Test
import org.junit.Assert._


class DispatcherTest {
//	@Test
	 def direct() : Unit = {
//	  val r = """Chat: --- (.+) (?:(:?was killed)|(?:has crashed)|(?:bailed out))\.\\n""".r
	   val r = """Chat: --- (.+) was killed|has crashed|bailed out\.\\n""".r
	  assertTrue("""Chat: --- entrop regulation was killed.\n""" match {
	    case r(who) => true
     case _ => false
	  })
	 }
	@Test  
	def testRegex() : Unit ={
		val d = new LocalizedDispatcher(){
		  override def conf = new Configuration("default.conf")
		} 
		d.updateConfiguration
  
  
  
//		d processLine """Chat: --- Pilot(entrop regulation) was killed.\n"""

		d learnNewName "entrop regulation"
  		d learnNewName "Mad"
  		d learnNewName "entrop regulationabcdefg"

    d processLine """\"""+"""u00201      entrop regulationabcdefg 1 0    (2)Blue     ZZ + AH     Ju-87D-5\n"""
    d processLine """\"""+"""u00201      entrop regulationabcdefg 1 0    (2)Blue     ZZ(9)sZa    Ju-87D-5\n"""
    
//  		
//  
//  		d processLine """Chat: --- Pilot(entrop regulation) was killed.\n"""
//
//  
//		d processLine """Chat: Mad: \ttest msg T s\n"""
//		d processLine "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
//		d processLine "\\"+"""u00201       Mad            1       0       (2)Blue     < +         Bf-109G-2\n"""
////Thread sleep 2000  
//		d processLine "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
//		d processLine "\\"+"""u00201       Mad            1       0       (2)Blue     red-2       Porsche\n"""
//		d processLine "\\"+"""u00201       Sturm0jetty    1999    -29000  (1)Red      \n"""
////Thread sleep 2000  
//		d processLine "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
//		d processLine "\\"+"""u00201       Mad            1       0       (2)Blue     red-2       IL-2\n"""
	}
}
 