package de.immaterialien.qlmap

import org.junit._
import org.junit.Assert._

class CsvParserTest {
  @Test
  def testMulti = {
   val result = CsvParser.loadMultiObjects("C:/Users/ulf/Desktop/fbdj","IL2MultiObjects.csv")
   val ZIS6_fuel = result.get("ZIS6_fuel") 
   assertTrue("has ZIS6_fuel in "+result, ZIS6_fuel.isDefined)
   assertTrue("ZIS6_fuel is fuel "+ZIS6_fuel, ZIS6_fuel.get==GroundClass.Fuel)
   println(result.mkString("\n"))
  }  
  @Test
  def testStatic = {
   def c(s:String)=assertTrue("static:"+s, CsvParser.parseAll(CsvParser.cell , s).successful)
   
   c(""""t"""" )
   c("""0""" )
   
   def t(s:String)={
     val r=CsvParser.parseAll(CsvParser.staticObjectsLine , s)
     assertTrue("static:"+s, r.successful)
//     println("r->"+r)
   }
   t(""""Tank","M4A2_76W_US","M4A2-76W US",0""")
   t(""""Tank","M4A2_76W_US","M4A2-76W US","0"""")
   t(""""AAA","BarrageBalloon_600m","Barrage Balloon",0""")
   t(""""AAA","BA_10","BA 10",0""")
    
   val result = CsvParser.loadStaticObjects("C:/Users/ulf/Desktop/fbdj","IL2StaticObjects.csv")
   val ZIS6_fuel = result.get("ZIS6_fuel") 
   assertTrue("has ZIS6_fuel in "+result, ZIS6_fuel.isDefined)
   assertTrue("ZIS6_fuel is fuel "+ZIS6_fuel, ZIS6_fuel.get==GroundClass.Fuel)
//   println(result.mkString("\n"))
  }  
}