package de.immaterialien.sturmonanny.util

import _root_.de.immaterialien.sturmonanny.util._
import scala.collection.mutable

import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._

trait Domain[D <: Domain[D]] extends Logging { self: D =>

  // public interface 
  def create(name: String): Unit = if (!(items contains name)) domainActor ! Create(name)
  def remove(name: String): Unit = domainActor ! Remove(name)
  def clear(): Unit = domainActor ! Clear
  def forElement(name: String)(body: (this.Element) => Unit): Unit = domainActor ! ForElement(name, body)
  def forMatches(patt: String)(body: (this.Element) => Unit): Unit = domainActor ! ForMatches(patt, body)

  val items: mutable.Map[String, this.Element] = new mutable.LinkedHashMap
  private object domainActor extends LiftActor {
    override def messageHandler: PartialFunction[Any, Unit] = {
      case p: Domain.this.Element => {
        //debug("domainactor adding  "+p.name)		     
        items.put(p.name, p)
      }
      case Unregister(p) => items.remove(p.name)

      case ForMatches(pat, body) => {
        val founds = find(pat)
        for (found <- founds) {
          //    	    find(pat) map ({found=>
          //debug("found: "+found.name+" applying "+body)    	    		
          body(found)

        }
      }
      case ForElement(name, body) => {
        val elem = items.getOrElseUpdate(name, newElement(name))
        body(elem)
      }
      case Remove(name)=>items remove name
      case Create(name)=>newElement(name)
      case Clear => items.clear
      case x => debug("unknown in domain " + this.getClass.getSimpleName + ": " + x)
    }
  }
  abstract class Element(val name: String) extends LiftActor with Logging { import java.io._
    val domain = Domain.this
    def toFileName(in:String)={
  		var v1=in
   		v1 = v1.replaceAll("""[^a-zA-Z\d\.\-_=|^@<>]+""", "_")
   		v1  	
  	}
    protected var messageLog : Option[Writer] = {
    	val fname = new java.io.File({
    		var v1 = this.getClass.getSimpleName
    		while (v1.endsWith("$")) v1 = v1.substring(0, v1.length-1)
    		val offs = v1.lastIndexOf("$")
    		if(offs>0) v1 = v1.substring(offs)
    		
    		toFileName(v1)
    	}+"."+toFileName(name)+".log")
System.err.println("might be logging to "+fname);    	
    	if( ! enableMessageLog) None else try{
    		Some(new BufferedWriter(new FileWriter(fname), 1024))
    	}catch{
    		case e => error("could not open fname message log", e)
    		None
    	}
    }
    def enableMessageLog = false  
    def remove() {
    	for(w<-messageLog) {
    		w.flush()
    		w.close()
    	}
    	messageLog=None    	
    }
    
    
    domainActor ! this
    def unknownMessage(x: Any) = debug(this.getClass.getSimpleName + " " + name + " got unidentified message " + x)
  }
  /**
   * implement! 
   */
  protected def newElement(name: String): this.Element
  /**
   * find elements based on a trivial "*" search pattern, result is lazy so do not expose to a different thread!
   */
  private def find(pat: String): Iterable[this.Element] = {
    if ((pat eq null) || pat.trim.isEmpty) {
      items map (_ _2)
    } else {
      def regexMatchings(reg: scala.util.matching.Regex) = items filterKeys (reg findFirstIn _ isDefined) map (_ _2)

      val content = """(\Q""" + pat.replaceAll("""\*""", """\\E.*\\Q""") + """\E)"""
      val unquoted = (content).r
      val quoted = ("^" + unquoted + "$").r
      var ret = regexMatchings(quoted)
      if (ret isEmpty) {
        ret = regexMatchings(unquoted)
        ret
      } else {
        ret
      }
    }
  }
  private case class Unregister(val who: Domain.this.Element)
  private case class ForMatches(val pat: String, body: (Domain.this.Element) => Unit)
  private case class ForElement(val name: String, body: (Domain.this.Element) => Unit)
  private case class Remove(val name:String)
  private case class Create(val name:String)
  private case object Clear
  //   	protected object PERSIST

}
