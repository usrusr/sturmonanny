package de.immaterialien.qlmap

object HtmlTest {
  def main(args: Array[String]) {

    val filter = new HtmlMissionFilter("src/main/resources/mapbase/out.conf")
    val misFile = new java.io.File("src/test/resources/Afrika_42194204050.mis")
    
    filter.invoke(misFile) 
  }
} 
class HtmlTest {
  @org.junit.Test
  def runMain = HtmlTest.main((""::Nil).toArray)
  
}