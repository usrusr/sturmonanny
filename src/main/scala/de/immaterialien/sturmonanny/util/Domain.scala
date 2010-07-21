package de.immaterialien.sturmonanny.util

import de.immaterialien.sturmonanny.util._
import scala.collection.mutable 

 
import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._
 

trait Domain[D <: Domain[D]]  extends  Logging{ 
 	self : D =>
  	
  	// public interface 
  	def create(name:String) :Unit = if( ! (items contains name)) domainActor ! newElement(name) 
	def remove(name:String) :Unit = domainActor ! remove(name)
	def forElement(name:String) ( body : (this.Element)=>Unit) :Unit = domainActor ! forElement(name, body) 
	def forMatches(patt:String)( body : (this.Element)=>Unit) :Unit = domainActor ! forMatches(patt, body) 
  
	val items : mutable.Map[String, this.Element] = new mutable.LinkedHashMap
	private object domainActor extends LiftActor {
		override def messageHandler : PartialFunction[Any,Unit] = {  
		  case p : Domain.this.Element => {
//debug("domainactor adding  "+p.name)		     
		    items.put(p.name, p)
		  }
		  case unregister(p) => items.remove(p.name)
	   
    	  case forMatches(pat, body) => {
    	    val founds = find(pat)
    	    for(found <- founds){
//    	    find(pat) map ({found=>
//debug("found: "+found.name+" applying "+body)    	    		
			    	  body(found)    	    
			
         }
    	  }
    	  case forElement(name, body) => {
    		  val elem = items.getOrElseUpdate(name, newElement(name))
    		  body(elem)
         }
	
		  case x => debug("unknown in domain "+this.getClass.getSimpleName+": "+x)   
		}
  	}
	abstract class Element(val name : String) extends LiftActor with Logging{
		val domain = Domain.this
		domainActor ! this
		def unknownMessage(x : Any) = debug(this.getClass.getSimpleName +" "+name + " got unidentified message "+ x)
	}
	/**
	 * implement! 
     */
	protected def newElement(name:String) : this.Element
	/**
	 * find elements based on a trivial "*" search pattern, result is lazy so do not expose to a different thread!
	 */
    private def find(pat:String) : Iterable[this.Element] = {
     if(( pat eq null) || pat.trim.isEmpty){
       items map (_ _2)
     }else{
    	def regexMatchings(reg : scala.util.matching.Regex) = items filterKeys (reg findFirstIn _ isDefined) map (_ _2)
    	 
      	val content = """(\Q"""+pat.replaceAll("""\*""", """\\E.*\\Q""")+"""\E)"""
      	val unquoted = (content).r
	    val quoted = ("^"+unquoted+"$").r
	     var ret = regexMatchings(quoted)
	     if(ret isEmpty){
	       ret = regexMatchings(unquoted)
			ret
	     }else{
	       ret
	     }
      }
   }
	private case class unregister(val who : Domain.this.Element)
    private case class forMatches(val pat:String, body : (Domain.this.Element) => Unit)
    private case class forElement(val name:String, body : (Domain.this.Element) => Unit)
//   	protected object PERSIST


}
