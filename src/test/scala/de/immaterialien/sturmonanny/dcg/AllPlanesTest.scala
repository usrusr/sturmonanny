package de.immaterialien.sturmonanny.dcg

import java.io._
import org.junit._

class AllPlanesTest {
	@org.junit.Test
 def test=AllPlanesTest.main(null)
}
object AllPlanesTest { 
	def main(args: Array[String]) { 
//		println(">>>>>>:"+
//		new de.immaterialien.qlmap.TalkingHtmlMissionFilter.Inline("E:/2.8workspace/qlmap/src/main/resources/mapbase/out.conf")
//		.getClass.getName)
		
		net.lag.configgy.Configgy.configure("log.conf")
//		val f0 = new File("src/test/resources/de/immaterialien/sturmonanny/dcg/Iasi44194405020.mis")
//		val f1 = new File("C:/zuti-IL2-server/Dedi/Missions/Net/dogfight/DCG/Iasi44194405010.mis.mis.preflatten")
		
//		val f0 = new File("src/test/resources/de/immaterialien/sturmonanny/dcg/failedSequence.orig.mis")
//		val f0 = new File("src/test/resources/italy_crash1.mis")
//		val f0 = new File("src/test/resources/hnmaps/italy_43194308120.mis")
//			val f0 = new File("src/test/resources/de/immaterialien/sturmonanny/dcg/sgrad_one_retreat_too_many.mis")	
//			val f0 = new File("src/test/resources/italy_not_an_int/italy_43194403310.mis")
//		val f0 = new File("src/test/resources/italy_not_an_int/staling_43194211240.mis")
//		val f0 = new File("src/test/resources/italy_not_an_int/otherBrokenChief.mis")
//		val f0 = new File("src/test/resources/italy_map_fail/italy_43194308180.mis.pre.ForceAiAirStart")
//		val f0 = new File("src/test/resources/okifail/okinawa_45194504050.mis")
		//val f0 = new File("src/test/normandy_44194407280.mis")
		val f0 = new File("src/test/normandy_44194407050.mis")
		
		
		
		val f1 = new File(f0.toString.dropRight(4)+".processed.mis") 
		f1.delete 
		org.apache.commons.io.FileUtils.copyFile(f0,f1)
		

//		val ap = new AllPlanesEverywhere("config")
//		val ap = new DoNothingMisRewriter("config")
//		val ap = 
//			new PimpMyBornPlace(" 1 1000 200 11 0 500 12000 30 0 0 0 0 0 3.8 0 0 0")
//			new AllPlanesEverywhere("config")
//			new DoNothingMisRewriter("config")
//			new DelayedChiefs("min=2 max=10")
//		ap.invoke(f1)
		val aps = List(
				
							
//			new ForceDotsInChiefs(""),
//			new ForceAiAirStart(""),	
//			new AllPlanesEverywhere("config"),
//			new PimpMyBornPlace(" 1 1000 200 11 0 500 12000 30 0 0 0 0 0 3.8 0 0 0"),
//			new DelayedChiefs("min=2 max=20"),				
//			
//			new RetreatBornPlace("distance=10000 radius=3000 minRemaining=2 " +
//					" bluedummy=2 vehicles.artillery.Artillery$Maxime bluedummy= 3 vehicles.artillery.Artillery$MG42"+
//					" reddummy=vehicles.artillery.Artillery$Maxime "
//			),
			
//			new de.immaterialien.qlmap.HtmlMissionFilter("E:/2.8workspace/qlmap/src/main/resources/mapbase/out.conf"),						
			new de.immaterialien.qlmap.HtmlMissionFilter.Inline("E:/2.8workspace/qlmap/src/main/resources/mapbase/out.conf"),						
			new DoNothingMisRewriter("config"){override def invoke(in:File)=in}
		)
		for(ap<-aps) ap.invoke(f1)
		
		println("-> "+f1.length+" bytes in "+f1)
	}
}