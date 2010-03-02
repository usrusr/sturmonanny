package de.immaterialien.sturmonanny.util.configgy

import scala.collection.mutable
import scala.xml._
import net.liftweb.common._
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._


/**
 * extends the ConfigurationSchema class with a liftForm method that creates the xhtml for a "live" view on the configuration
 * with bindings for "apply" and "save"
 */

trait LiftSupport extends ConfigurationSchema{
	private val self : ConfigurationSchema = this
	private val form = "cfg"
	def  liftForm:NodeSeq={
//println("entrering ilfgForm")
	  val topName = configgyName
	  def forMember(member: ConfigurationSchema.Member):(NodeSeq, List[BindParam])={
		  
	    member match {
		    case group : ConfigurationSchema#Group => { 
		      val internal = group.map(forMember _)
	       
		      val headline : Node = 
	        if(group.documentationString == null && group.documentationString.trim.isEmpty){
	          <div class="configgy.group.headline">{group.configgyName}</div>
	        }else{
	          <div class="configgy.group.headline" title={group.documentationString}>{group.configgyName}</div>
	        }
		      ( // create the tuple 
		        <div class="configgy.group">{headline}
	            {internal.map(_ _1)}
	          </div>
		        , internal.flatMap(_ _2)
		      )
		    }
		    case table : ConfigurationSchema#Table[_] => {
		      (<none/>, Nil)
		    } 
		    case field : ConfigurationSchema#Field[_] => {
		      val fieldVal = field.apply
		      val fullName = field.configgyPath + field.configgyName
		      var attributes : MetaData = Null
        //val t:net.liftweb.mapper.view.TableEditor = null
//        val t:net.liftweb.mapper.view.snippet.TableEditor = null  
		    	val binding = fieldVal match {
		    	  case x:String 	=> {
		    	    attributes = new UnprefixedAttribute("title", "Text", attributes)
		    	    fullName -> SHtml.text(x, field.asInstanceOf[ConfigurationSchema#Field[String]] () = _)
            }
		    	  case x:Boolean 	=> {
		    	    fullName -> SHtml.checkbox(x, field.asInstanceOf[ConfigurationSchema#Field[Boolean]] () = _)
		    	  }
		    	  case x:Int		 	=> {
		    		  attributes = new UnprefixedAttribute("title", "full number", attributes)
							fullName -> SHtml.text(x.toString, {d:String=>
		    	    	field.asInstanceOf[ConfigurationSchema#Field[Int]] () = (d.toInt)
		    	    })
		    	  }
		    	}
		    	
		    	//attributes = new UnprefixedAttribute("id", fullName, attributes)
		    	val fNode : Elem= new Elem(form, fullName, attributes, xml.TopScope)
		    	val unbound =
		    		if(field.documentationString==null || field.documentationString.trim.isEmpty) 
			    		<label class="configgy.label">{field.configgyName}{fNode}</label>
			    	else
			    		<label class="configgy.label" title={field.documentationString}>{field.configgyName}{fNode}</label>
		      (unbound, binding::Nil)
		    } 
	  }
	}
	  
	  
	
	  val params = new mutable.ListBuffer[BindParam]
		val tuples : Seq[(NodeSeq, List[BindParam])]= self.map{m =>
//println("fo member "+m)		  
		  forMember(m)
		}
                                
		var createdSeq : NodeSeq = Nil
    createdSeq = tuples.foldLeft(createdSeq){(existing, tuple)=>   
       params ++ tuple._2
		   existing ++ tuple._1 
		} 
		val save = new Elem(form, "submitsave", Null, xml.TopScope)
		val apply = new Elem(form, "submitapply", Null, xml.TopScope)
		val title = if(documentationString==null || documentationString.trim.isEmpty) 
                 <div class="configgy.form">{file}</div>
    else
                 <div class="configgy.form" title={documentationString}>{file}</div>
		val formNodes = <form>{title}
       	{createdSeq}
        {apply}
        {save}
    </form>
    params += "submitapply" -> SHtml.submit("Apply", (() => ()))
    params += "submitsave" -> SHtml.submit("Save", () => println("saved "+System.identityHashCode(self)+"\n"+self))
    
//println("println form "+System.identityHashCode(self)+"\n"+self )       
//println("form: "+formNodes)   
		var ret = bind(form, formNodes, params :_*)
		
//println("println ret")       
//println("ret: "+ret)   
    ret
	}
//println("members:")                                  
//for(m<-members){
//println("member "+m)		  
//}                                  
//println("/members:")   
}
