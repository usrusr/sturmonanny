package de.immaterialien.sturmonanny.util

import net.lag.configgy._

import _root_.de.immaterialien.sturmonanny.core.Configuration
import _root_.de.immaterialien.sturmonanny.util.configgy._
import org.junit.Test
import org.junit.Assert._
class ConfiggyTest {
  @org.junit.Test
  def test()=ConfiggyTest.main(null)
}
object ConfiggyTest { 
	
  def main(args : Array[String]) : Unit = {  
    val c = new Configuration("./src/test/resources/example.conf", null) 
    with LiftSupport
     
    println("names.currency before '"+(c.names.currency)+"'" )
    println("game.refund before "+(c.game.refund) )
    c.game.refund() = 20
    println("game.refund after "+(c.game.refund) ) 
    println("game.refund after "+(0  + c.game.refund.apply) )  
              println("parsed \n"+c)
              
    val pimp = c.fbdj.DCG.addonArguments("de_immaterialien_sturmonanny_dcg_PimpMyBornPlace")              
    assertTrue(pimp.startsWith(" "))
  }
}
