package de.immaterialien.gfxutil

import java.awt._
import java.awt.geom._
//package gfxutil{

//implicit def wrapG2D(in:Graphics2D) = new G2dWrapper(in)
//}
object Implicits {
  class G2dWrapper(g2d: Graphics2D) {
    /**
     * saves the transformation
     */
    def freeze(func: => Unit): Unit = {
      val frozen = g2d.getTransform
      try {
        func
      } finally {
//        println("resetting transform")
        g2d.setTransform(frozen)
      }
    }
  }

  implicit def g2dToFrozen(in: Graphics2D): G2dWrapper = new G2dWrapper(in)
}