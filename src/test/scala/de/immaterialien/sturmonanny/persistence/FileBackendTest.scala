package de.immaterialien.sturmonanny.persistence

import org.junit.Test
import org.junit.Assert._

class FileBackendTest {

	@Test
	def test {
		val first = new FileBackend()  
		val printInfo : String=>Unit = (msg=> println("info: "+msg))
		val failError : String=>Unit = (msg=> org.junit.Assert.fail("test error: "+msg))
		
		first.open(Map(), printInfo,failError )
		val f = new java.io.File("test.balance")
		assertTrue("file should either not exist or be deleted",(! f.exists)||f.delete)
		first.open(Map("file"->"test.balance"), (msg=>println("info: "+msg)), (msg=>assertEquals("file Some(test.balance) does not exist",msg)))

		val pilot1 = "pilot 1 ä \"\\\". /\\q@"
		val p1 = first.load(pilot1)
//		assertEquals(0d, p1.get.red, 0.0000000001d)
//		assertEquals(0d, p1.get.blue, 0.0000000001d)
		assertEquals(None, p1)
		
		first.store(pilot1, Some(1d), None)
		val p2 = first load pilot1
		assertEquals(0d, p2.get.blue, 0.0000000001d)
		assertEquals(1d, p2.get.red, 0.0000000001d)
		
		first.close((msg=>println("info "+msg)), (msg=>println("fail "+msg)))
		assertTrue("file should exist or be deleted",(f.exists))
		
		val second = new FileBackend()  
		second.open(Map("file"->"test.balance"), (msg=>println("info: "+msg)), (msg=>assertEquals("file Some(test.balance) does not exist",msg)))
		val p3 = second load pilot1
		def checkLine(parser:FileBackend.Parser.Parser[_], text:String){
			val psed = FileBackend.Parser.parse(parser , text)			
			val res = psed.successful
println("test:'"+(if(res)psed else "none")+"' from '"+text+"' -> "+parser)
			assertTrue("'"+text+"' should match "+parser, res)
		}
checkLine(FileBackend.Parser.pilot , "\"test\"")
checkLine(FileBackend.Parser.floatingPointNumber , "1.0d")

checkLine(FileBackend.Parser.line , """"pilot p1" = red: 1.0 blue: 0.0""")
checkLine(FileBackend.Parser.possiblyLine , """"pilot p1" = red : 1.0 blue: 0.0""")


		assertEquals("p3 should be defined", true, p3.isDefined)
		assertEquals(0d, p3.get.blue, 0.0000000001d)
		assertEquals(1d, p3.get.red, 0.0000000001d)
		
		
	}
}