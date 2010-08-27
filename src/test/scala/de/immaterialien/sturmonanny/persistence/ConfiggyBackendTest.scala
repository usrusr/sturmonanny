package de.immaterialien.sturmonanny.persistence

import org.junit.Test
import org.junit.Assert._

class ConfiggyBackendTest {

	@Test
	def test {
		val first = new ConfiggyBackend()
		first.open(Map(), (msg=>println("info: "+msg)), (msg=>fail("test error: "+msg))) 
		val f = new java.io.File("test.balance")
		assertTrue("file should either not exist or be deleted",(! f.exists)||f.delete)
		first.open(Map("file"->"test.balance"), (msg=>println("info: "+msg)), (msg=>assertEquals("file Some(test.balance) does not exist",msg)))

		val pilot1 = "pilot 1"
		val p1 = first.load(pilot1)
		assertEquals(0d, p1.get.red, 0.0000000001d)
		assertEquals(0d, p1.get.blue, 0.0000000001d)
		
		first.store(pilot1, Some(1d), None)
		val p2 = first load pilot1
		assertEquals(0d, p2.get.blue, 0.0000000001d)
		assertEquals(1d, p2.get.red, 0.0000000001d)
		
		
		
	}
}