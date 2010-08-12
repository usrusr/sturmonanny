package de.immaterialien.sturmonanny.dcg

import java.io._
import org.junit._

class AllPlanesTest {

}
object AllPlanesTest {
	def main(args: Array[String]) {
		val f1 = new File("src/test/resources/de/immaterialien/sturmonanny/dcg/Iasi44194405020.mis")
		val f2 = new File(f1.getAbsolutePath+".flat.mis")
		if(f2.exists) f2.delete
		val ap = new AllPlanesEverywhere("config")
		
		ap.invoke(f1)
		
		println("-> "+f2.length+" bytes")
	}
}