package de.immaterialien.qlmap

object HtmlTest {
  def main(args: Array[String]) {

    val filter = new HtmlMissionFilter("E:/2.8workspace/qlmap/src/main/resources/mapbase/out.conf")
    var misFile = new java.io.File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194401240.mis")
//    misFile = new java.io.File("E:/IL2-server/Dedi/Missions/Net/dogfight/DCG/Italien_43194401240.mis")
    
    filter.invoke(misFile) 
  }
} 
class HtmlTest {
  @org.junit.Test
  def runMain = HtmlTest.main((""::Nil).toArray)
  
}