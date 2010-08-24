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
        ctx.setDynamicState(BridgeContext.STATIC)
        val builder = new GVTBuilder()

//doc.getDocumentElement.setAttributeNS(null, "overflow", "visible");        
        
        val rootGN = builder.build(ctx, doc)
//// removes clipping, but does not help with the rotate-cut       
//val treeWalker = new org.apache.batik.gvt.GVTTreeWalker(rootGN)
//var currNode = treeWalker.nextGraphicsNode
//
//  while (currNode != null) {
//          currNode.setClip(null)
//          currNode = treeWalker.nextGraphicsNode
//  }

        
        
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
    paint(trans, depth, g2d, 10)
    paint(trans, depth, g2d, -10)
  }
    
  def paint(trans: AffineTransform, depth: Double, g2d: java.awt.Graphics2D, r1:Int) {
//    var old = r.getTransform
//    if(old == null) old = new AffineTransform()
    val nt = 
//      old
//      new AffineTransform(old); nt.concatenate(trans)
      new AffineTransform
//      nt.concatenate(trans)
//    r.setTransform(nt)
      val halfW =  (dimensions._1 /2)
      val halfH =  (dimensions._2 /2)
//      nt.translate(-halfW, -halfH)
//      nt.rotate(100,4,halfW, halfH)
//)
      val sx=trans.getScaleX
      val sy=trans.getScaleY
      
      //nt.translate(-halfW/sx, -halfH/sy)
      //nt.rotate(r1,0, -trans.getTranslateX/sx, -trans.getTranslateY/sy)

   nt.translate(halfW, halfH)
      
      nt.scale(sx, sy)
//      nt.rotate(-0.01,r1)
//      nt.rotate(r1,-0.1) 
      //nt.translate(-halfW*sx, -halfH*sy)
      nt.translate(-halfW, -halfH)
      //nt.translate(halfW, halfH)
//      r.setClip(null)
      //r.setTransform(nt)
//  r.
    
      g2d freeze {
//        trans.invert

  
//        g2d.setTransform(the)
        g2d.setRenderingHint(RenderingHintsKeyExt.KEY_TRANSCODING, RenderingHintsKeyExt.VALUE_TRANSCODING_PRINTING)
//  g2d.rotate(0.03, trans.getTranslateX, trans.getTranslateY)
//        nt.concatenate(AffineTransform.getTranslateInstance(trans.getTranslateX, trans.getTranslateY))
//  g2d.translate(trans.getTranslateX, trans.getTranslateY)
//        g2d.setTransform(nt)
  
//  val rcat = new AffineTransform
//  rcat.concatenate(nt)
//  rcat.rotate(r1, 4)
//  
//  rcat.preConcatenate(AffineTransform.getTranslateInstance(trans.getTranslateX, trans.getTranslateY))
  
//  var m=r.getMask
//  if(m!=null){
//  val mr:geom.Rectangle2D = m.getFilterRegion
//  mr.add(-halfW, -halfH)
//  mr.add(2*halfW, 2*halfH)
//  m.setFilterRegion(mr)
//  r.setMask(m)
//  }
        
        val treeWalker = new org.apache.batik.gvt.GVTTreeWalker(r)
var currNode = treeWalker.nextGraphicsNode

  while (currNode != null) {
//          currNode.setClip(null)
//          currNode.setMask(null)
    val filter = currNode.getFilter
if(filter!=null){
  println("  filter "+filter)
}
          currNode.setFilter(null)
          currNode = treeWalker.nextGraphicsNode
  }
        
//println("r is a "+r.getClass.getCanonicalName+ " bounds: "+r.getBounds)
//val b = r.getBounds
//b.add(0-(sx*halfW), 0-(sy*halfH))
//b.add(0+(sx*halfW), 0+(sy*halfH))
//b.add(b.getX-(sx*halfW), b.getY-(sy*halfH))
//b.add(b.getX+(sx*halfW)+b.getWidth, b.getY+(sy*halfH)+b.getHeight)
        val tempg = g2d.create((trans.getTranslateX - 2*halfW).toInt, (trans.getTranslateY- 2*halfH).toInt, (4*halfW).toInt, (4*halfH).toInt)
//        tempg.clipRect((trans.getTranslateX - 2*halfW).toInt, (trans.getTranslateY- 2*halfH).toInt, (4*halfW).toInt, (4*halfH).toInt)
        val tempg2d = tempg.asInstanceOf[Graphics2D]
        tempg2d.rotate(0.39, halfW, halfW)
        tempg2d.clipRect(-(1*halfW).toInt,-(1*halfW).toInt,(4*halfW).toInt, (5*halfH).toInt)
        
        val cloneBounds = r.getBounds.clone.asInstanceOf[Rectangle2D]
        val b = r.getBounds
//        val p1 = new Point2D.Double(-halfW, -halfH)
//        val p2 = new Point2D.Double(3*halfW, 3*halfH)
//        val p3 = new Point2D.Double(3*halfW, -halfH)
//        val p4 = new Point2D.Double(-halfW, 3*halfH)
        
        val p1 = new Point2D.Double(b.getX, b.getY)
        val p2 = new Point2D.Double(b.getX, b.getY+b.getHeight)
        val p3 = new Point2D.Double(b.getX+b.getWidth, b.getY)
        val p4 = new Point2D.Double(b.getX+b.getWidth, b.getY+b.getHeight)
//        p.
        
        r.setTransform(new AffineTransform)
        //r.getTransform.rotate(100,1, trans.getTranslateX, trans.getTranslateY)
//        val tg2t = tempg2d.getTransform
        val tg2t = r.getTransform
        
        val tp1 = tg2t.transform(p1, null)
        val tp2 = tg2t.transform(p2, null)
        val tp3 = tg2t.transform(p3, null)
        val tp4 = tg2t.transform(p4, null)
        
        val miX = Math.min(Math.min(tp1.getX, tp2.getX), Math.min(tp3.getX, tp4.getX))
        val miY = Math.min(Math.min(tp1.getY, tp2.getY), Math.min(tp3.getY, tp4.getY))
        val maX = Math.max(Math.max(tp1.getX, tp2.getX), Math.max(tp3.getX, tp4.getX))
        val maY = Math.max(Math.max(tp1.getY, tp2.getY), Math.max(tp3.getY, tp4.getY))
        val difX = maX-miX
        val difY = maY-miY
         
//        b.setFrame(
//            (miX-difX).toInt, (miY-difY).toInt,
//            (difX*4).toInt, (difY*4).toInt
//        )
//        tempg2d.clipRect(-(1*halfW).toInt,-(1*halfW).toInt,(4*halfW).toInt, (5*halfH).toInt)
        
        r.paint(tempg2d)
//        r.paint(g2d)
//        b.setRect(cloneBounds)

        // removes clipping, but does not help with the rotate-cut       

        
        tempg2d.dispose
      }
//    nt.invert
//    r.setTransform(new AffineTransform)
//     nt.concatenate(trans)
//    r.setTransform(r.getInverseTransform)
  }
  def dimensions: (Int, Int) = {
    val bds = r.getBounds
    if(bds==null) (64,64) else (bds.getWidth.toInt, bds.getHeight.toInt)
  }
}