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

object ScalableSprite {
  import org.apache.batik.ext.awt.image.renderable._
  val parser = batik.util.XMLResourceDescriptor.getXMLParserClassName
  
  val domImpl = svg.SVGDOMImplementation.getDOMImplementation
  
  val factory = new svg.SAXSVGDocumentFactory(parser, false)
  
  factory.setValidating(false)
  val hueRotation = Math.Pi.toFloat / 3f


  def create(cls: GC, side: Int, mapBaseFolder:Option[File]): Option[Paintable] = {
    val fname: String = (cls match {
      case Artillery => Car.toString
      case Plane => "Airfield"
      case x => x toString
    }) + ".svg"

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
//        implicit val doc:SVGDocument = domImpl.createDocument(svg.SVGDOMImplementation.SVG_NAMESPACE_URI, "svg" , null).asInstanceOf[SVGDocument]
        
        
        val defs : SVGDefsElement= doc.getDocumentElement.asInstanceOf[SVGSVGElement].firstElement("defs").map(_.asInstanceOf[SVGOMDefsElement]).getOrElse{
          new SVGOMDefsElement(null, doc.asInstanceOf[batik.dom.AbstractDocument])
        }
        val fi:SVGFilterElement = new SVGOMFilterElement(null, doc.asInstanceOf[batik.dom.AbstractDocument])
        defs.append(fi)
        fi.setId("MyFilter")
        val fecol = fi.appended(new SVGOMFEColorMatrixElement(null, doc.asInstanceOf[batik.dom.AbstractDocument]))
        fecol.setAttribute("type", "matrix")
        val matrix = {
          val a = "0.2"
          def r = if(side==1) " 1 " else " 0 "
          def b = if(side==2) " 1 " else " 0 "
          val o = " 0 "
          val l = " 1 "

                      ""  
          b+r+o+" 0    "+a+ " " +
          r+o+b+" 0    "+a+ " " +
          o+b+r+" 0    "+a+ " " +
//          "1 -2 1 1      0" +
          "0 -2 0 1.4      0" +
          ""
          
//          "1 0 0 0 0 " +
//          "0 1 0 0 0 " +
//          "0 0 1 0 0 " +
//          "0 0 0 1 0"
        }
        fecol.setAttribute("values", matrix);
        {
          val d = doc.getDocumentElement
          var n :SVGElement = doc.getRootElement

          
          val g = {
            val cs = n.getChildNodes
            for(i <- 0 until cs.getLength)yield{
              val c = cs.item(i)
              c
            }
          }.filter(_.getNodeName=="g").first.asInstanceOf[Element]
          g.setAttribute("filter","url(#MyFilter)")
        }
//println(dumpNode(doc))        

        val userAgent = new UserAgentAdapter()
        val loader = new DocumentLoader(userAgent)
        val ctx = new BridgeContext(userAgent, loader)
        ctx.setDynamicState(BridgeContext.DYNAMIC)
        val builder = new GVTBuilder()

        val rootGN = builder.build(ctx, doc)
//        rootGN.
//        val f : Option[ColorMatrixRable]= 
//          if (side == 1) Some(ColorMatrixRable8Bit.buildHueRotate(hueRotation * -1)) 
//          else if (side == 2) Some(ColorMatrixRable8Bit.buildHueRotate(hueRotation)) 
//          else None
//        
//        var rable:AbstractRable = new GraphicsNodeRable8Bit(rootGN) 
//        rable = {
//            val r = ColorMatrixRable8Bit.buildSaturate(4f).asInstanceOf[AbstractRable]
//            r.asInstanceOf[ColorMatrixRable].setSource(rable)
//            r
//          }
//        
//        val res = f map (f=>{
//          f.setSource(rable)
//          f
//        }) getOrElse rable
//        
//        
//        Some(new RableSprite(res))
        
        
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
class RableSprite(r: RenderableImage) extends Log with Paintable {
  import ScalableSprite._

  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D) {
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//    val i = r.
    g2d.drawRenderableImage(r, trans)
//    g2d.drawRenderedImage(r, trans)
  }
  def dimensions: (Int, Int) = {
//    val bds = r.getDimension
    (r.getWidth.toInt, r.getHeight.toInt)
  }
}

class ScalableSprite(r: batik.gvt.GraphicsNode) extends Log with Paintable {
  import ScalableSprite._

  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D) {
    
    var old = r.getTransform
    if(old == null) old = new AffineTransform()
    val nt = 
      old
      new AffineTransform(old); nt.concatenate(trans)
     
    r.setTransform(nt)
    
    g2d.setRenderingHint(RenderingHintsKeyExt.KEY_TRANSCODING, RenderingHintsKeyExt.VALUE_TRANSCODING_PRINTING)

    
    r.paint(g2d)
    nt.invert
    r.setTransform(new AffineTransform)
  }
  def dimensions: (Int, Int) = {
    val bds = r.getBounds
    if(bds==null) (64,64) else (bds.getWidth.toInt, bds.getHeight.toInt)
  }
}