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
import org.apache.batik.ext.awt.image.renderable._
import org.apache.batik.gvt.filter._
import java.awt.image.renderable._
import java.io._
import org.w3c.dom._
import org.w3c.dom.svg._
import DomHelper._

object ScalableSprite {
  import org.apache.batik.ext.awt.image.renderable._
  val parser = batik.util.XMLResourceDescriptor.getXMLParserClassName
  val factory = new svg.SAXSVGDocumentFactory(parser, false)
  factory.setValidating(false)
  val hueRotation = Math.Pi.toFloat / 3f


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
        implicit val doc:SVGDocument = factory.createSVGDocument(null, stram)

        val matrix = {
          val a = "0.3"
          def r = if(side==1) " 1 " else " 0 "
          def b = if(side==2) " 1 " else " 0 "
          val o = " 0 "
          val l = " 1 "
//          ""  
//          b+r+o+o+a+ "\n" +
//          r+o+b+o+a+ "\n" +
//          o+b+r+o+a+ "\n" +
//          o+o+o+l+o+ "\n" +
//          ""
            
                      ""  
          b+r+o+" 0    "+a+ " " +
          r+o+b+" -0.2 "+a+ " " +
          o+b+r+" 0    "+a+ " " +
          "1 -1 1 1 0" +
          ""
        }
        {
          val d = doc.getDocumentElement
          var n :SVGSVGElement = doc.getRootElement
//          val children : NodeList = n.getChildNodes
//          val defs = doc.createElement("defs")
//          val len = children.getLength
//          val fs = children.item(1)
println("n children: "+n.children("g").length)
          val defs = n.firstElement("defs").getOrElse{
            val d=doc.element("defs")
            n.appendBefore("g", d) 
            d
          }
          defs.append(
                  doc.element("filter",
//                      "filterUnits"-> "userSpaceOnUse",
//                      "x"-> "0",
//                      "y"-> "0",
//                      "height"-> "64",
//                      "width"-> "64",
                      "id"->"MyFilter"
                  ).append(
                      doc.element(
                          "feColorMatrix", 
                          "type"->"matrix",
                          "values"->matrix 
                      )
                  )
              )
          
          
          //children.
          //n.insertBefore(n.getChildNodes.item(0), defs)
//          val myf = doc.createElement("filter")
//          myf.setAttribute("id", "MyFilter")
//          myf.setAttribute("filterUnits","userSpaceOnUse")
//          myf.setAttribute("x","0" ) 
//          myf.setAttribute("y","0" ) 
//          myf.setAttribute("width","64" ) 
//          myf.setAttribute("height","64")
//          defs.appendChild(myf)
          
//          val gb = doc.createElement("feGaussianBlur")
//          gb.setAttribute("in", "SourceAlpha") 
//          gb.setAttribute("stdDeviation", "4") 
//          gb.setAttribute("result", "blur")
//          myf.appendChild(gb)
          
          val g = {
            val cs = n.getChildNodes
            for(i <- 0 until cs.getLength)yield{
              val c = cs.item(i)
              c
            }
          }.filter(_.getNodeName=="g").first.asInstanceOf[Element]
          g.setAttribute("filter","url(#MyFilter)")
        }
println(dumpNode(doc))        

        val userAgent = new UserAgentAdapter()
        val loader = new DocumentLoader(userAgent)
        val ctx = new BridgeContext(userAgent, loader)
        ctx.setDynamicState(BridgeContext.DYNAMIC)
        val builder = new GVTBuilder()

        val rootGN = builder.build(ctx, doc)
//        rootGN.
        val f : Option[ColorMatrixRable]= 
          if (side == 1) Some(ColorMatrixRable8Bit.buildHueRotate(hueRotation * -1)) 
          else if (side == 2) Some(ColorMatrixRable8Bit.buildHueRotate(hueRotation)) 
          else None
        
        var rable:AbstractRable = new GraphicsNodeRable8Bit(rootGN) 
        rable = {
            val r = ColorMatrixRable8Bit.buildSaturate(4f).asInstanceOf[AbstractRable]
            r.asInstanceOf[ColorMatrixRable].setSource(rable)
            r
          }
        
        val res = f map (f=>{
          f.setSource(rable)
          f
        }) getOrElse rable
        
        
        
        
        Some(new ScalableSprite(rootGN))
//        Some(new RableSprite(res))
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
//
    val nt = 
      old
      new AffineTransform(old); nt.concatenate(trans)
      
    //        new AffineTransform(trans);nt.concatenate(old)
    //r.setFilter()
//    r.getDocument
//    r.setFilter(f)
      
    r.setTransform(nt)
    
    r.paint(g2d)
    r.setTransform(old)
  }
  def dimensions: (Int, Int) = {
    val bds = r.getBounds
    if(bds==null) (64,64) else (bds.getWidth.toInt, bds.getHeight.toInt)
  }
}