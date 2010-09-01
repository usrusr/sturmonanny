package de.immaterialien.qlmap.sprites
import org.apache.batik.dom.svg._
import java.io._
import de.immaterialien.qlmap._

import java.awt._
import java.awt.geom._
import image._
import javax.imageio.ImageIO
import GroundClass._
import scala.collection._
import org.apache.batik
import batik.dom.svg
import batik.ext.awt.image.renderable._
import batik.gvt.filter._
import batik.ext.awt.RenderingHintsKeyExt
import java.awt.image.renderable._
import java.io._
import org.w3c.dom._
import org.w3c.dom.svg._
import DomHelper._

import de.immaterialien.gfxutil.Implicits._

object ScalableSprite {
  import org.apache.batik.ext.awt.image.renderable._
  val parser = batik.util.XMLResourceDescriptor.getXMLParserClassName

  val domImpl = svg.SVGDOMImplementation.getDOMImplementation

  val factory = new svg.SAXSVGDocumentFactory(parser, false)

  factory.setValidating(false)
  val hueRotation = math.Pi.toFloat / 3f

  def create(cls: GC, side: Int, mapBaseFolder: Option[File]): Option[Paintable] = {
    val fname: String = (cls match {
      case Artillery => Car.toString
      case Plane => "Airfield"
      case x => x toString
    }) + ".svg"

    def filterMatrix = {
      val a = "0.2"
      def r = if (side == 1) " 1 " else " 0 "
      def b = if (side == 2) " 1 " else " 0 "
      val o = " 0 "
      val l = " 1 "

      ""
      b + r + o + " 0    " + a + " " +
        r + o + b + " 0    " + a + " " +
        o + b + r + " 0    " + a + " " +
        //          "1 -2 1 1      0" +
        "0 -2 0 1.4      0" +
        ""

      //          "1 0 0 0 0 " +
      //          "0 1 0 0 0 " +
      //          "0 0 1 0 0 " +
      //          "0 0 0 1 0"
    }

    log debug ("getting " + fname)
    //Sprites.getClass.classPathResource(fname)
    try {
      //val stram = Sprites.getClass.getResourceAsStream("/de/immaterialien/qlmap/sprites/" + fname)
      val stram = mapBaseFolder map { base =>
        new FileInputStream(new File(base, fname))
      } getOrElse ScalableSprite.getClass.getResourceAsStream("/de/immaterialien/qlmap/sprites/" + fname)
      log debug ("stream " + stram)
      try {
        import batik.bridge._
        implicit val doc = factory.createDocument(null, stram).asInstanceOf[SVGDocument]
        val batikDoc = doc.asInstanceOf[batik.dom.AbstractDocument]
        //        implicit val doc:SVGDocument = domImpl.createDocument(svg.SVGDOMImplementation.SVG_NAMESPACE_URI, "svg" , null).asInstanceOf[SVGDocument]

        val defs: SVGDefsElement = doc.getDocumentElement.asInstanceOf[SVGSVGElement].firstElement("defs").map(_.asInstanceOf[SVGOMDefsElement]).getOrElse {
          new SVGOMDefsElement(null, batikDoc)
        }
        val fi: SVGFilterElement = new SVGOMFilterElement(null, batikDoc)
        defs.append(fi)

        fi.setId("MyFilter")
        val fecol = fi.appended(new SVGOMFEColorMatrixElement(null, batikDoc))
        fecol.setAttribute("type", "matrix")
        val matrix = filterMatrix
        fecol.setAttribute("values", matrix);
        {
          val d = doc.getDocumentElement
          var n: SVGElement = doc.getRootElement

          val go = {
            val cs = n.getChildNodes
            for (i <- 0 until cs.getLength) yield {
              val c = cs.item(i)
              c
            }
          }.filter(_.getNodeName == "g").headOption.asInstanceOf[Option[Element]]
          if(go.isDefined) log warn "no group found below root element, rotation might be limited"
          n.setAttribute("filter", "url(#MyFilter)")

          val ge = new SVGOMGElement(null, batikDoc)
          ge.setAttribute("id", "nannyrotator")
          n.pushBetweenChildren(ge)
        }

        val userAgent = new UserAgentAdapter()
        val loader = new DocumentLoader(userAgent)
        val ctx = new BridgeContext(userAgent, loader)
        ctx.setDynamicState(BridgeContext.DYNAMIC)
        val builder = new GVTBuilder()

        val rootGN = builder.build(ctx, doc).asInstanceOf[batik.gvt.RootGraphicsNode]
      
        Some(new ScalableSprite(rootGN, side))
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
class RableSprite(r: RenderableImage) extends Log with Paintable {
  import ScalableSprite._

  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D, rotation:Option[(Double, Double)]=None) {
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //    val i = r.
    
    val t = new AffineTransform(trans)
    t.translate(dimensions._1/ -2,dimensions._2/ -2)
    for(r<-rotation) t.rotate(r._1, r._2, t.getTranslateX, t.getTranslateY)
    
    g2d.drawRenderableImage(r, trans)
    //    g2d.drawRenderedImage(r, trans)
  }
  def dimensions: (Int, Int) = {
    //    val bds = r.getDimension
    (r.getWidth.toInt, r.getHeight.toInt)
  }
}

class ScalableSprite(r: batik.gvt.RootGraphicsNode, side: Int) extends Log with Paintable {
  import ScalableSprite._

  val originalBounds = r.getPrimitiveBounds.clone.asInstanceOf[Rectangle2D]
  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D, rotation:Option[(Double, Double)]=None) {

    var old = r.getTransform
    if (old == null) old = new AffineTransform()
    val nt = new AffineTransform()

    nt.concatenate(trans)
    val halfW = (dimensions._1 / 2)
    val halfH = (dimensions._2 / 2)

    nt.translate(-halfW, -halfH)

    for(r<-rotation) rotateToVector(r._1, r._2)
    r setTransform nt

    g2d.setRenderingHint(RenderingHintsKeyExt.KEY_TRANSCODING, RenderingHintsKeyExt.VALUE_TRANSCODING_PRINTING)

    r.paint(g2d)
    nt.invert
    r.setTransform(old)
  }
  /**
   * sets a local rotation around the center of the filter region that is applied to the inner "nannyrotate" node 
   * so the outer filter region remains unaffected this will rotate "round" shapes without clipping but anything that 
   * goes outside the biggest possible circle inside the filter may get clipped 
   * 
   * @param x
   * @param y
   */
  private def rotateToVector(x: Double, y: Double): Unit = {
    import scala.collection.JavaConversions._
  
    val (halfX, halfY) = 
//      (dimensions._1/2, dimensions._2/2)
    {
      val max = math.max(dimensions._1, dimensions._2)/2
      (max,max)
    }
//      {
//        val bds = r.getBounds
//        (bds.getWidth / 2, bds.getHeight / 2)
//      }
      
println("rotating "+halfX+","+halfY)        
    val rot = AffineTransform.getRotateInstance(x, y, halfX, halfY)
    for (c <- r.getChildren.map(_.asInstanceOf[batik.gvt.GraphicsNode])) {
      if (c.isInstanceOf[batik.gvt.CompositeGraphicsNode]) {
        for (cc <- c.asInstanceOf[batik.gvt.CompositeGraphicsNode].getChildren.map(_.asInstanceOf[batik.gvt.GraphicsNode])) {
          cc.setTransform(rot)
        }
      }
    }
  }
  /**
   * manipulate all filter nodes in a gvt tree
   * @param func
   */
  private def forFilters[T <: Filter](func: T => T) {
    val treeWalker = new org.apache.batik.gvt.GVTTreeWalker(r)
    var currNode = treeWalker.nextGraphicsNode

    while (currNode != null) {
      val filter: Filter = currNode.getFilter

      if (filter != null && filter.isInstanceOf[T]) {
        currNode.setFilter(func(filter.asInstanceOf[T]))
      }
      currNode = treeWalker.nextGraphicsNode
    }
  }
  def dimensions: (Int, Int) = {
    val bds = r.getBounds
    if (bds == null) (64, 64) else (bds.getWidth.toInt, bds.getHeight.toInt)
  }
}