package de.immaterialien.sturmonanny.fbdjhosting

import org.junit.Test
import org.junit.Assert._

class FbdjHostTest {
  
		@Test 
		def test():Unit={
//		  val exception = try{
//			  val nonExisting = new FbdjHost("nonexisting", "C:\\Users\\ulf\\.m_2\\repository\\de\\immaterialien\\FBDj-overrides\\0.0.1-SNAPSHOT\\FBDj-overrides-0.0.1-SNAPSHOT.jar", "bla")
//			  null
//			}catch{
//			  case x => x
//			}
//			assertTrue(exception.getClass == classOf[java.io.FileNotFoundException])
   
			val existing = new FbdjHost("C:\\Users\\ulf\\Desktop\\fbdj", "C:\\Users\\ulf\\.m2\\repository\\de\\immaterialien\\FBDj-overrides\\0.0.1-SNAPSHOT\\FBDj-overrides-0.0.1-SNAPSHOT.jar", "confPath")
    
   
			() 
		}
}
