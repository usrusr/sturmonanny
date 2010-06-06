package de.immaterialien.sturmonanny.core

import org.junit.Test
import org.junit.Assert._


class StatsDispatcherTest {

	@Test  
	def testRegex() : Unit ={
		val d = new LocalizedDispatcher(){
		  override def conf = new Configuration("default.conf", null)
		  override def pilotMessageSend(who:String, what:Is.Event) = debug("would have sent to "+who+"<-"+what) 
		} 
		d.updateConfiguration
		
		d processMessage testString
  
		println("n->"+(d processMessage """Chat: --- Pilot(entrop regulationabcdefg) has crashed.\n
"""))
 
    ()
	}
 
 def testString = """-------------------------------------------------------\n
Name: \tentrop regulationabcdefg\n
Score: \t0\n
State: \tKIA\n
Enemy Aircraft Kill: \t0\n
Enemy Static Aircraft Kill: \t0\n
Enemy Tank Kill: \t0\n
Enemy Car Kill: \t0\n
Enemy Artillery Kill: \t0\n
Enemy AAA Kill: \t0\n
Enemy Wagon Kill: \t0\n
Enemy Ship Kill: \t0\n
Friend Aircraft Kill: \t0\n
Friend Static Aircraft Kill: \t0\n
Friend Tank Kill: \t0\n
Friend Car Kill: \t0\n
Friend Artillery Kill: \t0\n
Friend AAA Kill: \t0\n
Friend Wagon Kill: \t0\n
Friend Ship Kill: \t0\n
Fire Bullets: \t0\n
Hit Bullets: \t0\n
Hit Air Bullets: \t0\n
Fire Roskets: \t0\n
Hit Roskets: \t0\n
Fire Bombs: \t5\n
Hit Bombs: \t0\n
-------------------------------------------------------\n
Name: \tusrusr\n
Score: \t-600\n
State: \tLanded at Airfield\n
Enemy Aircraft Kill: \t0\n
Enemy Static Aircraft Kill: \t0\n
Enemy Tank Kill: \t0\n
Enemy Car Kill: \t0\n
Enemy Artillery Kill: \t0\n
Enemy AAA Kill: \t0\n
Enemy Wagon Kill: \t0\n
Enemy Ship Kill: \t0\n
Friend Aircraft Kill: \t2\n
Friend Static Aircraft Kill: \t0\n
Friend Tank Kill: \t0\n
Friend Car Kill: \t0\n
Friend Artillery Kill: \t0\n
Friend AAA Kill: \t0\n
Friend Wagon Kill: \t0\n
Friend Ship Kill: \t0\n
Fire Bullets: \t1652\n
Hit Bullets: \t1199\n
Hit Air Bullets: \t1183\n
Fire Roskets: \t0\n
Hit Roskets: \t0\n
Fire Bombs: \t0\n
Hit Bombs: \t0\n
-------------------------------------------------------\n""".replaceAll("""\Q\r\n\E""", """\Q\n\E""")   
}
 