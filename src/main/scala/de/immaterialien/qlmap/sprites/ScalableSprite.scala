package de.immaterialien.qlmap.sprites

import de.immaterialien.qlmap._

import java.awt._
import java.awt.geom._
import image._
import javax.imageio.ImageIO
import GroundClass._
import scala.collection._
import org.apache.batik
import batik.dom.svg

import java.io._

object ScalableSprite {
  import org.apache.batik.ext.awt.image.renderable._
  val parser = batik.util.XMLResourceDescriptor.getXMLParserClassName
  val factory = new svg.SAXSVGDocumentFactory(parser, false)
  factory.setValidating(false)
  val toRed = Some(
      ColorMatrixRable8Bit.buildHueRotate(Math.Pi.toFloat / 3f)
      
  )
  val toBlue = Some(ColorMatrixRable8Bit.buildHueRotate(Math.Pi.toFloat / -3f))

  def create(cls: GC, side: Int): Option[Paintable] = {
    val fname: String = (cls match {
      case Artillery => Car.toString
      case Plane => "Airfield"
      case x => x toString
    }) + ".svg"

    log debug ("getting " + fname)
    //Sprites.getClass.classPathResource(fname)
    try {
      val stram = Sprites.getClass.getResourceAsStream("/de/immaterialien/qlmap/sprites/" + fname)
      log debug ("stream " + stram)
      try {
        import batik.bridge._
        //val doc = factory.createDocument("http://immaterialien.de/uri/qlmap/sprites#" + fname, stram)
        val doc = factory.createDocument(null, stram)

        val userAgent = new UserAgentAdapter()
        val loader = new DocumentLoader(userAgent)
        val ctx = new BridgeContext(userAgent, loader)
        ctx.setDynamicState(BridgeContext.DYNAMIC)
        val builder = new GVTBuilder()
        
        
        val rootGN = builder.build(ctx, doc)
//        rootGN.
        val f = if (side == 1) toRed else if (side == 2) toBlue else None
        f map (f=>{
          val af = rootGN.getFilter
          f.setSource(af)
          rootGN.setFilter(f)
        })
        Some(new ScalableSprite(rootGN))
      } catch {
        case e => {
          error("error loading " + fname, e)
          None
        }
      }
    } catch {
      case e => {
        error("failed to load " + fname, e)
        None
      }
    }
  }

}
class ScalableSprite(r: batik.gvt.GraphicsNode) extends Log with Paintable {
  import ScalableSprite._

  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D) {
    
    var old = r.getTransform
    if(old == null) old = new AffineTransform()
//
    val nt = 
      old
      new AffineTransform(old); nt.concatenate(trans)
      
    //        new AffineTransform(trans);nt.concatenate(old)
    //r.setFilter()
    r.setTransform(nt)
    r.paint(g2d)
    r.setTransform(old)
  }
  def dimensions: (Int, Int) = {
    val bds = r.getBounds
    if(bds==null) (64,64) else (bds.getWidth.toInt, bds.getHeight.toInt)
  }
}