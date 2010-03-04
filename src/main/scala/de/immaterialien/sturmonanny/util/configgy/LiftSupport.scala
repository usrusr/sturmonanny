package de.immaterialien.sturmonanny.util.configgy

import scala.collection.mutable
import scala.xml._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.lag.configgy

/**
 * extends the ConfigurationSchema class with a liftForm method that creates the xhtml for a "live" view on the configuration
 * with bindings for "apply" and "save"
 */

trait LiftSupport extends ConfigurationSchema {
	private val self : ConfigurationSchema = this
	private val form = "cfg"
 
	object intermediates extends SessionVar[configgy.Config](new configgy.Config)
 
	def  liftForm:NodeSeq={
println("entrering ilfgForm ")


	  val topName = name
	  def forMember(member: ConfigurationSchema.Member):(NodeSeq, List[BindParam])={
		  
	    member match {
		    case group : ConfigurationSchema#Group => { 
		      val internal = group.map(forMember _)
	       
		      val headline : Node = 
	        if(group.documentationString == null && group.documentationString.trim.isEmpty){
	          <div class="configgy.group.headline">{group.name}</div>
	        }else{
	          <div class="configgy.group.headline" title={group.documentationString}>{group.name}</div>
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
		      val fullName = field.full
		      var attributes : MetaData = Null

		    	val binding = fieldVal match {
//		    	  case x:String 	=> {
//		    	    attributes = new UnprefixedAttribute("title", "Text", attributes)
//		    	    fullName -> SHtml.text(x, field.asInstanceOf[ConfigurationSchema#Field[String]] () = _)
//            }
//		    	  case x:Boolean 	=> {
//		    	    fullName -> SHtml.checkbox(x, field.asInstanceOf[ConfigurationSchema#Field[Boolean]] () = _)
//		    	  }
//		    	  case x:Int		 	=> {
//		    		  attributes = new UnprefixedAttribute("title", "full number", attributes)
//							fullName -> SHtml.text(x.toString, {d:String=>
//		    	    	field.asInstanceOf[ConfigurationSchema#Field[Int]] () = (d.toInt)
//		    	    })
//		    	  }
       
       		  case x:String 	=> {
		    	    attributes = new UnprefixedAttribute("title", "Text", attributes)
		    	    fullName -> new StringValidator(field.asInstanceOf[ConfigurationSchema#Field[String]])
            }
		    	  case x:Boolean 	=> {
		    	    fullName -> new BooleanValidator(field.asInstanceOf[ConfigurationSchema#Field[Boolean]])
		    	  }
		    	  case x:Int		 	=> {
		    		  attributes = new UnprefixedAttribute("title", "full number", attributes)
							fullName -> new IntegerValidator(field.asInstanceOf[ConfigurationSchema#Field[Integer]])
		    	  }
       
		    	}
		    	
		    	//attributes = new UnprefixedAttribute("id", fullName, attributes)
		    	val fNode : Elem= new Elem(form, fullName, attributes, xml.TopScope)
		    	val unbound =
		    		if(field.documentationString==null || field.documentationString.trim.isEmpty)  
			    		<label class="configgy.label">{field.name}{fNode}</label>
			    	else
			    		<label class="configgy.label" title={field.documentationString}>{field.name}{fNode}</label>
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
    
    val paramCopy:List[Validator[_]] = params.map{
      case TheBindableBindParam(_, x:Validator[_]) => Some(x)
      case _ => None
 		}.filter(_ isDefined).map(_ get).toList
    
    params += "submitapply" -> SHtml.submit("Apply", () => updateConfiguration(paramCopy) )
    params += "submitsave" -> SHtml.submit("Save", () => {
	      if(updateConfiguration(paramCopy)) 
	    	  println("saved "+System.identityHashCode(self)+"\n"+self)
	    	else
	    		println("did not save, errors\n")
    })
    
//println("println form "+System.identityHashCode(self)+"\n"+self )       
//println("form: "+formNodes)   
		var ret = bind(form, formNodes, params :_*)
		
//println("println ret")       
//println("ret: "+ret)   
    ret
	}
	def updateConfiguration(paramCopy:List[Validator[_]]):Boolean = {
	  if(paramCopy.exists(_.validate.isDefined)){
	    false
	  }else{
	    self.apply(intermediates)
	    true
    }
	}
 
	private abstract class Validator[T](receiver : ConfigurationSchema#Field[T] ) extends net.liftweb.util.Bindable{
    def updateIntermediates(update:String) = {
      
println("receiver.full:"+receiver.full+" update:"+update+"\ninterms bef:"+intermediates )
      intermediates(receiver.full) = update
println("interms aft:"+intermediates )
    }
    /**
     * return Some("myError") in case of failure
     */
	 	def validate : Option[String] = None 
	 	 
	 	def current : String = {
	 	  intermediates(receiver.full, receiver.apply.toString)
    }
	 	def currentOpt : Option[String] = {
	 	  intermediates.getString(receiver.full)
    }	
   
    def validationNodes(nodes:NodeSeq)={
    	validate match {
    	  case None => nodes
    	  case Some(err) => <div class="configgy.validation.msg">{nodes}{err}</div>
    	}
      
    }
    if(receiver.apply != null && receiver.apply.toString.trim.length>0) updateIntermediates(current)
	 	//def asHtml = SHtml.text(current, receiver () = _)
 	}
  private class StringValidator(receiver : ConfigurationSchema#Field[String] ) extends Validator[String](receiver){
    override def asHtml = validationNodes(SHtml.text(current, updateIntermediates _))

    override def validate = {
      currentOpt match {
        case Some(x) => {
          if(receiver.maxLength!=null && (x.length > receiver.maxLength.intValue)) Some("'"+x+"' is longer than the allowed maximum of "+receiver.maxLength+"!")
          else if(receiver.pattern!=null && ! receiver.pattern.unapplySeq(x).isDefined) Some("'"+x+"' does not match the regex pattern "+receiver.pattern+"!")
          None
        }
        case None => None
      }
    }
  }
  private class IntegerValidator(receiver : ConfigurationSchema#Field[Integer] ) extends Validator[Integer](receiver){
    override def asHtml = validationNodes(SHtml.text(current, updateIntermediates _))
    override def validate = {
      currentOpt match {
//        case LiftSupport.numberPattern(is) => {
//          val i:Int = is.toInt  
//        }
        case Some(is) => {
        	if( ! LiftSupport.numberPattern.unapplySeq(is).isDefined) Some("'"+is+"' is not a full number!")   
        	else {
        		val i = is.toInt
        	  if(receiver.max!=null && (i > receiver.max.intValue )) Some(""+i+" is greater than the allowed maximum of "+receiver.max+"!")
        	  else if(receiver.min!=null && (i < receiver.min.intValue )) Some(""+i+" is lower than the allowed minimum of "+receiver.max+"!")
        	  else None
          }
          
        }
        case None => None
      }
    }
  }
  private class BooleanValidator(receiver : ConfigurationSchema#Field[Boolean] ) extends Validator[Boolean](receiver){
    override def asHtml = {
      validationNodes (
      	SHtml.checkbox(current.toBoolean, x=>updateIntermediates(x.toString))
      )
    }
    override def validate = {
      currentOpt match {
        case Some("true") => None 
        case Some("false") => None 
        case None => None
        case Some(x) => Some("'"+x+"' is not a valid value true or false!")
      }
    }
  }
}
object LiftSupport {
  val numberPattern = """-?\d+""".r
}
