package de.immaterialien.qlmap.sprites

import java.awt.image._
import java.awt.geom._


trait Paintable {
  /**
   * @param trans
   * @param depth 0..1
   * @param g2d
   */
  def paint(trans:AffineTransform, depth:Double, g2d:java.awt.Graphics2D)
  def dimensions:(Int, Int)
}