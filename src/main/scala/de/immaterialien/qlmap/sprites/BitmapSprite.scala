package de.immaterialien.qlmap.sprites

import de.immaterialien.qlmap._
import java.awt.image._
import javax.imageio.ImageIO
import GroundClass._
import scala.collection._
import java.awt.geom._

object BitmapSprite extends Log {
  def create(cls: GC, side: Int):Option[Paintable] = {
    val infix = if (side == 1) ".Red" else if (side == 2) ".Blue" else ""
 
    val fname: String = (cls match {
      case Artillery => Car.toString
      case Plane => "Airfield"
      case x => x toString
    }) + infix + ".png"

    log debug ("getting " + fname)
    //Sprites.getClass.classPathResource(fname)
    val stram = Sprites.getClass.getResourceAsStream("/de/immaterialien/qlmap/sprites/" + fname)
    log debug ("stream " + stram)
    val iio = try {
      Some(ImageIO.read(stram))
    } catch {
      case _ => {
        log debug ("not found: " + fname)
        None
      }
    }
    iio map (x=>new BitmapSprite(x))
  }

}
class BitmapSprite(img:BufferedImage) extends Paintable with Log {
  def dimensions: (Int, Int) = (img.getWidth, img.getHeight)

  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D) {
    g2d.drawImage(img, trans, null)
    //        val vpadding = 16
    //        val hpadding = 3
    //        ig2.setColor(Color.black)
    //        ig2.drawString(count +"" , hpadding+randx-w2, vpadding+randy-h2)
  }
}