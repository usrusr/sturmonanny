package de.immaterialien.sturmonanny.util

import net.lag.configgy._
import de.immaterialien.sturmonanny
import sturmonanny.core.Configuration
 
object ConfiggyTest { 
  def main(args : Array[String]) : Unit = { 
    val c = new Configuration("E:/eclipseworkspace/sturmonanny/src/test/resources/example.conf")
     
    println("game.refund before "+(c.game.refund) )
    c.game.refund() = 20
    println("game.refund after "+(c.game.refund) ) 
    println("game.refund after "+(0+c.game.refund) )
              println("parsed \n"+c)
  }
}
