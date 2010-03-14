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
//println("entrering ilfgForm ")


	  val topName = name
	  def forMember(member: ConfigurationSchema.Member):(NodeSeq, List[BindParam])={
		  
	    member match {
		    case group : ConfigurationSchema#Group => { 
		      val internal = group.map(forMember _)
	       
		      val headline : Node = 
	        if(group.documentationString == null && group.documentationString.trim.isEmpty){
	          <div class="configgy_group_headline">{group.name}</div>
	        }else{
	          <div class="configgy_group_headline" title={group.documentationString}>{group.name}</div>
	        }
		      ( // create the tuple 
		        <div class="configgy_group">{headline}
		        	<div>
		        		{internal.map(_ _1)}
              </div>
	          </div>
		        , internal.flatMap(_ _2)
		      )
		    }
		    case table : ConfigurationSchema#Table[_] => {

		      val fullName = table.full
		      val fNode : Elem= new Elem(form, fullName, Null, xml.TopScope)
		      val binding = table.defaultValue match {
		        case x:String 	=> {
		    	    fullName -> new StringTabulator(table.asInstanceOf[ConfigurationSchema#Table[String]])
            }
		    	  case x:Boolean 	=> {
		    	    fullName -> new BooleanTabulator(table.asInstanceOf[ConfigurationSchema#Table[Boolean]])
		    	  }
		    	  case x:Int		 	=> {
							fullName -> new IntegerTabulator(table.asInstanceOf[ConfigurationSchema#Table[Integer]])
		    	  }
		      }
		      val unbound =
		    		if(table.documentationString==null || table.documentationString.trim.isEmpty)  
			    		(<div class="configgy_label">{table.name}</div><div class="configgy_textarea">{fNode}</div>)
			    	else
			    		(<div class="configgy_label" title={table.documentationString}>{table.name}</div><div class="configgy_textarea">{fNode}</div>)
//println("tablulator "+unbound)           
		      (<div class="configgy_field">{unbound}</div>, binding::Nil)
		    } 
		    case field : ConfigurationSchema#Field[_] => {
		      val fieldVal = field.apply
		      val fullName = field.full
		      var attributes : MetaData = Null
		      
		    	val binding = fieldVal match {
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
			    		(<div class="configgy_label">{field.name}</div><div class="configgy_textfield">{fNode}</div>)
			    	else
			    		(<div class="configgy_label" title={field.documentationString}>{field.name}</div><div class="configgy_textfield">{fNode}</div>)
		      (<div class="configgy_field">{unbound}</div>, binding::Nil)
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
                 <div class="configgy_fname">{file}</div>
    else
                 <div class="configgy_fname" title={documentationString}>{file}</div>
		val formNodes = <form class="configgy_form">{title}
       	{createdSeq}
        <div class="configgy_controls">
	        <div class="configgy_apply">{apply}</div>
	        <div class="configgy_save">{save}</div>
        </div>
    </form>
    
    val paramCopy:List[Bindator] = params.map{
      case TheBindableBindParam(_, x:Bindator) => Some(x)
      case _ => None
 		}.filter(_ isDefined).map(_ get).toList
    
    params += "submitapply" -> SHtml.submit("Apply", () => updateConfiguration(paramCopy) )
    params += "submitsave" -> SHtml.submit("Save", () => {
	      if(updateConfiguration(paramCopy)) 
	    	  println("saved "+System.identityHashCode(self)+"\n"+self)
	    	else
	    		println("did not save, errors\n")
    })
    
//println("println form "+System.identityHashCode(self)+"\n"+self+"\n   paramCopy:"+paramCopy )       
//println("form: "+formNodes)   
		var ret = bind(form, formNodes, params :_*)
		
//println("println ret")       
//println("ret: "+ret)   
    ret
	}
	def updateConfiguration(paramCopy:Seq[Bindator]):Boolean = {
	  val validationResult = paramCopy.flatMap(_ validate).toList
	  if(validationResult.isEmpty){
	    self.apply(intermediates)
	    true
	  }else{
	    false
    }
	}
  
	private trait  Bindator {
	  def validate : Iterable[String] 
   
	  def validationNodes(nodes:NodeSeq) : NodeSeq={
       
    	val ret = validate.map{ err : String => 
    	  <p>{err}</p>
    	}.toList
    	
      if(ret.isEmpty) nodes
      else <div class="configgy_validation">{nodes}<div class="configgy_validation_msg">{ret}</div></div>
    }
   
	}
 
	private abstract class Validator[T](receiver : ConfigurationSchema#Field[T] ) extends net.liftweb.util.Bindable with Bindator{
    def updateIntermediates(update:String) = {
      
//println("receiver.full:"+receiver.full+" update:"+update+"\ninterms bef:"+intermediates )
      intermediates(receiver.full) = update
//println("interms aft:"+intermediates )
    }
    /**
     * return Some("myError") in case of failure
     */
	 	def validate : Iterable[String] = None 
	 	 
	 	def current : String = {
	 	  intermediates(receiver.full, receiver.apply.toString)
    }
	 	def currentOpt : Option[String] = {
	 	  intermediates.getString(receiver.full)
    }	
   

    if(receiver.apply != null && receiver.apply.toString.trim.length>0) updateIntermediates(current)
	 	//def asHtml = SHtml.text(current, receiver () = _)
 	}
	
                                                                          
                                                                          
  private class StringValidator(receiver : ConfigurationSchema#Field[String] ) extends Validator[String](receiver){
    override def asHtml = {
//println("asHtml for  "+receiver.full)      
      val attributes = if(receiver.maxLength!=null) {
        ("size", ""+receiver.maxLength) :: Nil
      } else {
        Nil
      }
      validationNodes(SHtml.text(current, updateIntermediates _, attributes :_*))
    }

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
    override def asHtml = {
      val attributes = if(receiver.max!=null || receiver.min!=null) {
    	  val biggest : Int = if(receiver.max==null){
    	    receiver.min.intValue
    	  }else if (receiver.min==null){
    	    receiver.max.intValue
    	  }else{ 
    	    val max : Int = receiver.max.intValue 
    	    scala.Math.max(max, (-1 * receiver.min.intValue ))
    	  }
      	val len = 1 + (""+biggest).length
        ("size", ""+len) :: Nil
      } else {
        Nil
      }
      validationNodes(SHtml.text(current, updateIntermediates _, attributes:_*))
    }
    override def validate = {
      currentOpt match {
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
                   
                                                                                                            
                                                                                                            
                                                                                                            
                                                                                                            
                                                                                                            
                                                                                                            
  private abstract class Tabulator[T](receiver : ConfigurationSchema#Table[T] ) extends net.liftweb.util.Bindable with Bindator{
    def updateIntermediates(update:String) = {
      
//println("tab receiver.full:"+receiver.full+" update:"+update+"\ninterms bef:"+intermediates )
			for(line <- update.lines){
				line match {
				  case LiftSupport.tablePattern(name, value) => {
				    intermediates(receiver.full+"."+name) = value
				  }
				  case _ => 
        }
			}
//println("tab interms aft:"+intermediates )
    }
    /**
     * return Some("myError") in case of failure
     */
	 	def validateSeq : Seq[Option[String]] = {
	 	  //intermediates.getConfigMap(receiver.full).map(_.asMap.projection.toList.map(_ _2)).map(validate(_))
	 	  mapKeyValue{(k:String,v:String)=>
	 	    validate(v).map(k+": "+ _ )
	 	  }
	 	}  
	 	def validate(toValidate : String) : Option[String]
	  override def validate : Iterable[String] = {
	    validateSeq.filter(_ isDefined).map(_ get)  
	  }	 
   
	 	def current : String = { 
   		currentSeq.reverse.mkString("\r\n")
    }
	 	def currentSeq : Seq[String] = {
	 	  mapKeyValue((k:String,v:String)=> ""+ k + " = " + v)
//	 	  intermediates.getConfigMap(receiver.full).map{x => 
//   			x.asMap.projection.toList.map(kv=> ""+ kv._1 + " = " + kv._2)
//      }.getOrElse(Nil)
    }	
   	def mapKeyValue[R](func : ((String, String)=>R)):Seq[R] = {
   	  intermediates.getConfigMap(receiver.full).map{x => 
   			x.asMap.projection.toList.map(kv=> func(kv._1, kv._2))
      }.getOrElse(Nil)
   	}
   
   
    if(receiver.map != null && receiver.map.size>0) {
      receiver.map.foreach{entry=>
//println("entry:  "+entry._1+" -> "+entry._2)        
        intermediates(receiver.full+"."+entry._1) = entry._2.toString
      }
      updateIntermediates(current)
    }
	 	//def asHtml = SHtml.text(current, receiver () = _)
    
    override def asHtml = {
      validationNodes (
      	SHtml.textarea(current, x=>updateIntermediates(x)) 
      )
    }    
 	}
	                                                                                                        

                                                                           
  private class StringTabulator(receiver : ConfigurationSchema#Table[String] ) extends Tabulator[String](receiver){


    override def validate(x : String) = {
          if(receiver.maxLength!=null && (x.length > receiver.maxLength.intValue)) Some("'"+x+"' is longer than the allowed maximum of "+receiver.maxLength+"!")
          else if(receiver.pattern!=null && ! receiver.pattern.unapplySeq(x).isDefined) Some("'"+x+"' does not match the regex pattern "+receiver.pattern+"!")
          None
    }
  }
  private class IntegerTabulator(receiver : ConfigurationSchema#Table[Integer] ) extends Tabulator[Integer](receiver){

    override def validate(is : String) = {
    	if( ! LiftSupport.numberPattern.unapplySeq(is).isDefined) Some("'"+is+"' is not a full number!")   
    	else {
    		val i = is.toInt
    	  if(receiver.max!=null && (i > receiver.max.intValue )) Some(""+i+" is greater than the allowed maximum of "+receiver.max+"!")
    	  else if(receiver.min!=null && (i < receiver.min.intValue )) Some(""+i+" is lower than the allowed minimum of "+receiver.max+"!")
    	  else None
      }
    }
  }
  private class BooleanTabulator(receiver : ConfigurationSchema#Table[Boolean] ) extends Tabulator[Boolean](receiver){

    override def validate(toMatch:String) = {
      toMatch match {
        case "true" => None 
        case "false" => None 
        case x => Some("'"+x+"' is not a valid value true or false!")
      }
    }
  }                                                                           
                                                                           
                                                                           
}
object LiftSupport {
  val numberPattern = """-?\d+""".r
  val tablePattern = """^([^=\s]+)\s*=\s*(.*)$""".r
}
