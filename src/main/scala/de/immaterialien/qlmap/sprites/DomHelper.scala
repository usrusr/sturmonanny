package de.immaterialien.qlmap.sprites

import scala.collection._
import org.apache.batik
import batik.dom.svg
import org.apache.batik.ext.awt.image.renderable._
import org.apache.batik.gvt.filter._
import java.awt.image.renderable._
import org.w3c.dom._
import org.w3c.dom.svg._

object DomHelper {
  //  implicit def nodlister(nl:NodeList):Seq[Element]={
  //    new Seq[Element]{
  //      
  //    }
  //    List()
  //  }

  class WrapElement(e: Element, doc: SVGDocument) {
    def children(name: (Element => Boolean)): IndexedSeq[Element] = {
      val cs = e.getChildNodes
      val l = cs.getLength
      val ll = for (i <- 0 until l) yield {
        val n = cs.item(i)
        if (n.getNodeType == Node.ELEMENT_NODE) {
          val ne = n.asInstanceOf[Element]
          if (name(ne)) Some(ne) else None
        } else None 
      }
      ll flatten 
    }
    def domElement = e
    def append(es: Element*) = {
      for (c <- es) e.appendChild(c)
      this
    }
    def element(name: String, args: (String, String)*): Element = DomHelper.element(doc, name, args: _*)

    def append(name: String, args: (String, String)*) = {
      val c = element(name, args: _*)
      e.appendChild(c) 
      c
    }
    def appended[T<:Element](e:T):T={
      append(e)
      e
    }

    def firstElement(name: (Element => Boolean)): Option[Element] = {
      val cs = children(name)
      if (cs.length > 0) Some(cs.first) else None
    }
    def appendBefore(name: (Element => Boolean), es: Element*) = {
      firstElement(name).map { f =>
        for (c <- es.reverse) {
//          println("f is " + f)
//          println("e is " + e)
//          println("c is " + c)
          e.insertBefore(c, f)
        }
      } orElse {
        for (c <- es) e.appendChild(c)
        None
      }
      this
    }
  }

  def element(doc: SVGDocument, nsName: (String, String), args: (String, String)*):Element = {
    val e = doc.createElementNS(nsName._1, nsName._2).asInstanceOf[Element]
    for ((n, v) <- args) e.setAttribute(n, v)
    e
  }
  def element(doc: SVGDocument, name: String, args: (String, String)*) = {
    val e = doc.createElementNS(svg.SVGDOMImplementation.SVG_NAMESPACE_URI, name).asInstanceOf[Element]
    for ((n, v) <- args) e.setAttribute(n, v)
    e
  }

  implicit def nameFunc(name: String): (Element => Boolean) = _.getNodeName == name
  implicit def wrapElement(e: Element)(implicit doc: SVGDocument) = new WrapElement(e, doc)
  implicit def wrapElement(e: SVGElement)(implicit doc: SVGDocument) = new WrapElement(e, doc)
  implicit def unwrapElement(w: WrapElement): Element = w.domElement
  implicit def wrapDoc(doc: SVGDocument): DomHelper = new DomHelper(doc)
  //  implicit

  def dumpNode(doc: Node) = try {
    import javax.xml.transform._
    import java.io._
    val transformer = TransformerFactory.newInstance().newTransformer();

    transformer.setOutputProperty(OutputKeys.METHOD, "xml");
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
    transformer.setOutputProperty(OutputKeys.INDENT, "yes");
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
    transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");

    val sw = new StringWriter();
    val sr = new stream.StreamResult(sw);
    val domSource = new dom.DOMSource(doc);
    transformer.transform(domSource, sr);
    sw.toString

  } catch {
    case e => e.printStackTrace()
  }
}
class DomHelper(doc: SVGDocument) {
  def element(name: String, args: (String, String)*) = DomHelper.element(doc, name, args: _*)
  def element(nsName: (String, String), args: (String, String)*) = DomHelper.element(doc, nsName, args: _*)

}