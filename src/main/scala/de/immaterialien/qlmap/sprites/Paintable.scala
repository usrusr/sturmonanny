package de.immaterialien.qlmap.sprites

import java.awt.image._
import java.awt.geom._


trait Paintable {
  /**
   * @param trans transformation with translate & scale, but without rotation (avoid trouble with rotated batik filters)
   * @param depth 0..1
   * @param g2d
   * @param rotation optional local rotation, defined by "new x vector"
   */
  def paint(trans:AffineTransform, depth:Double, g2d:java.awt.Graphics2D, rotation:Option[(Double, Double)]=None)
  def dimensions:(Int, Int)
}