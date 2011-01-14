package de.immaterialien.qlmap

object HtmlTest { 
  def main(args: Array[String]) { 
net.lag.configgy.Configgy.configure("log.conf")
    //val filter = new HtmlMissionFilter("E:/2.8workspace/qlmap/src/main/resources/mapbase/out.conf")
//  	val filter = new HtmlMissionFilter("src/main/resources/mapbase/out.conf")
  	val filter = new HtmlMissionFilter("E:/2.8workspace/qlmap/src/main/resources/mapbase/out.configgy.conf")
  	
//    var misFile = new java.io.File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194401240.mis")
//    misFile = new java.io.File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194401240.mis")
//    misFile = new java.io.File("src/test/resources/de/immaterialien/sturmonanny/dcg/Iasi44194405020.mis")
  	for(mis <- List(
  	
//"src/test/resources/nbmaps/Iasi44194405020.mis", 
//"src/test/resources/nbmaps/Iasi44194405030.mis", 
//"src/test/resources/nbmaps/Iasi44194405040.mis", 
//"src/test/resources/nbmaps/Iasi44194405050.mis", 
//"src/test/resources/nbmaps/Iasi44194405060.mis", 
//"src/test/resources/nbmaps/Iasi44194405070.mis", 
//"src/test/resources/nbmaps/Iasi44194405080.mis", 
//"src/test/resources/nbmaps/Iasi44194405090.mis", 
//"src/test/resources/nbmaps/Iasi44194405100.mis", 
//"src/test/resources/nbmaps/Iasi44194405110.mis", 
//"src/test/resources/nbmaps/Iasi44194405120.mis", 
//"src/test/resources/nbmaps/Iasi44194405130.mis", 
//"src/test/resources/nbmaps/Iasi44194405140.mis", 
//"src/test/resources/nbmaps/Iasi44194405150.mis", 
//"src/test/resources/nbmaps/Iasi44194405160.mis", 
//"src/test/resources/nbmaps/Iasi44194405170.mis", 
//"src/test/resources/nbmaps/Iasi44194405180.mis", 
//"src/test/resources/nbmaps/Iasi44194405190.mis", 
//"src/test/resources/nbmaps/Iasi44194405200.mis", 
//"src/test/resources/nbmaps/Iasi44194405210.mis",

//"src/test/resources/de/immaterialien/qlmap/Iasi44194405011.mis",  			
//"src/test/resources/de/immaterialien/sturmonanny/dcg/failedSequence.orig.processed.mis",  			
"src/test/resources/nbmaps/Iasi44194405123.mis",
  	"").filter(_.length>0)){
  		val misFile = new java.io.File(mis)
  		filter.inline(misFile) 
  	}
  }
} 
class HtmlTest {
  @org.junit.Test 
  def runMain = HtmlTest.main(("src/test/resources/de/immaterialien/qlmap/Iasi44194405011.mis"::Nil).toArray)
  //misFile = new java.io.File("src/test/resources/de/immaterialien/sturmonanny/dcg/Iasi44194405020.mis")
}