package de.immaterialien.sturmonanny.util

import net.lag.configgy._
import de.immaterialien.sturmonanny

object ConfiggyTest {
  def main(args : Array[String]) : Unit = {
    val c = new sturmonanny.model.Configuration("test", "E:/eclipseworkspace/sturmonanny/src/test/resources/example.conf")
    
    println("game.refund before "+c.game.refund)
    
    println("game.refund after "+c.game.refund)
  }
}
