package de.immaterialien.sturmonanny.util.configgy

import net.lag.configgy.{Config, ConfigMap}

 
/**
 * 
 * 
 * base class for type-safe configurations consisting of a number of 
 * Group objects that are containing 
 * Field objects which are bound to a 
 * type (Int, Boolean, String) by providing a default value as constructor argument
 * 
 * features built-in serialization to "XML-style" configgy with field ordering based 
 * on the definition in the schema (using dirty reflection hacks) and inline documentation
 * 
 * <p/>usage example:
 * 
 * <code>
 * class MyConfig(file:String) extends ConfiggyFile(file){
 *	object myFirstGroup extends Group{ 
 *	  object hostExample extends Field( "127.0.0.1") 
 *	  object portExample extends Field(2001)   	  
 *	}
 *	object zombie extends Group{ 
 *	  object eats extends Field( "brains")
 *	}
 * }
 * </code>
 * 
 * which would expose a configuration like 
 * 
 * <code>
 * 
 * <MyConfig>
 *  <myFirstGroup>
 *   hostExample = "example.com"
 *   portExample = 80
 *  </myFirstGroup>
 *  <zombie>
 *   eats = "the living"
 *  </zombie>
 * 
 * </code> 
 *
 * possible member object types are:
 *   Group (nesting of other member object types),
 *   Field (holds a value)
 *   Table (exposes child nodes as a map with a fixed value type, identified by a default value)
 *   Documentation (holds nothing but a constant String)
 * 
 * 
 */
abstract class ConfigurationSchema(val file : String) extends Holder with ConfigurationSchema.Selfdocumenting{ 
    val initialized : Boolean = try{
 		this(Config.fromFile(file))
 		true
	}catch { 
	  case x => {
	    println("failed to read configuration file '"+file+"': "+x)
	    false
      }
	}
    
	def apply(conf : Config) = {
	  initMembers
	 // members map (_ readConfiggy conf)
	  for(member<-members) member.readConfiggy(conf)
	}

	override def toString = {
	  initMembers
	  val sb = new scala.StringBuilder 
//println("->> main ")
      write(sb, "", "")
	  sb toString
	}

 	initMembers

 	/**
 	 * Groups really are just Holders (of other groups or fields) and Members (of other Groups or ConfiggyFile) 
     */
//	protected[ConfigurationSchema] 
  trait Group extends Holder with ConfigurationSchema.Member {
	  override def toString = "group "+name
	}
 

// 
//	/**
//     *  a helper to add documentation to the definition source file 
//     *  that gets serialized by the ConfiggyFile
//     */
//     protected[ConfigurationSchema] class Documentation( override val v : String ) extends DocMember(v)
//	protected trait Doc extends Member {
//	    override protected[configgy] lazy val documentation = Some(new DocMember(doc))
//	    def doc : String
//	}
 
    /**
     * key/value pairs of a certain type, we don't just dive into the configgy ConfigMap because we want 
     * the types, the default values and the ability to merge a new Config into an existing ConfigFile
     */
//	protected[ConfigurationSchema] 
	class Table[T]( var v : T ) extends ConfigurationSchema.Member with ValidationInfo{
	  val defaultValue = v
	  var map = Map[String, T]()
	  def apply(what:String) : T = map.get(what) getOrElse defaultValue
	  def update(what:String, value:T) = map = map + ((what, value))
	  val extractor = v match {
     	case x:Int => (cMap : ConfigMap, k:String, oldValue:T)=>cMap(k,oldValue.asInstanceOf[Int]).asInstanceOf[T]
     	case x:String => (cMap : ConfigMap, k:String, oldValue:T)=>cMap(k,oldValue.asInstanceOf[String]).asInstanceOf[T]
     	case x:Boolean => (cMap : ConfigMap, k:String, oldValue:T)=>cMap(k,oldValue.asInstanceOf[Boolean]).asInstanceOf[T]
     
	    case _ => (cMap : ConfigMap, k:String, oldValue:T)=>{
	      oldValue
	    }
	  } 

      def printer = defaultValue match {
	    case _:String=> (what:T)=>{"\""+what+"\""}
	    case _ => (what:T)=>{""+what}
	  }
	  override def readConfiggy(in:Config){
	  	    in.getConfigMap(full) foreach {cMap =>
	  	    	var nMap = map
            	for(k <- cMap.keys) {
            	  val oldV : T = Table.this.map.get(k) getOrElse defaultValue
            	  val newV = extractor(cMap, k, oldV)
            	  if(newV != defaultValue){
            		  nMap = nMap  + ((k, newV))
            	  }
            	}
	  	    	map = nMap
	  	    }
	  	        
	  }
	    override def write(sb : scala.StringBuilder, indent : String, prefix : String){
//	      documentation foreach (_ write(sb, indent, prefix))
    	writeDocumentation(sb, indent, prefix)
		  sb.append(indent+"<"+ name+">\r\n")
		  innerTable(sb, indent)
		  sb.append(indent+"</"+ name+">\r\n")
	    }
     override def toString = "table "+name+":"+map
     def innerTable(sb : scala.StringBuilder, indent:String) = for((k,v)<-map.projection) sb.append(indent+"   "+k+" = "+printer(v) +"\r\n")
     def innerTable : String = {
       val sb = new scala.StringBuilder()
       innerTable(sb, "")
       sb.toString
     }
	} 

    class Field[T]( var v : T ) extends ConfigurationSchema.Member with ValidationInfo{
	    def update(t:T)={v = t}
	    def apply = v
      

	
	    override def readConfiggy(in:Config) = v match{
		      case x : String => v = in(full, x).asInstanceOf[T]  
		      case x : Int => v = in(full, x).asInstanceOf[T]  
		      case x : Boolean => v = in(full, x).asInstanceOf[T]  
    	}
	
	    override def toString = v.toString
	    override def write(sb : scala.StringBuilder, indent : String, prefix : String){
	      //documentation foreach (_.write(sb, indent, prefix))
	      writeDocumentation(sb, indent, prefix)
	      def string = v match {
		    case x:String=> "\""+x+"\""
		    case x => x
		  }
		  sb.append(indent+ name+"="+string+"\r\n")
	    }
	  }


    trait ValidationInfo {
      /**
       * applies to string 
       * plain nullable is easier to setup/define
       * (only enforced in LiftSupport)
       */
	    protected[configgy] var pattern : scala.util.matching.Regex = null
      /**
       * applies to string 
       * (only enforced in LiftSupport)
       */
	    protected[configgy] var maxLength : Integer = null
      /**
       * apply to int 
       * (only enforced in LiftSupport)
       */
	    protected[configgy] var min : Integer = null
	    protected[configgy] var max : Integer = null
    }
 }
 	
/**
 * ordering helper based on a little reflective magic stack-trace magic, needs stack traces to work!
 */

 	sealed trait SelfNaming extends Ordered[SelfNaming]{
 	  	
//      def configgyName = name
//      def configgyPath = prefix
//      lazy val configgyFullName = prefix+name
//      
   	  protected[configgy] lazy val full = {
		    val tmp = this.getClass.getSimpleName()
		    val dotsTmp = tmp.replace("$", ".")
		    if(dotsTmp contains "."){
		      dotsTmp.substring(0, dotsTmp.length - 1)
		    }else{
		      dotsTmp
		    }
   	  }
      protected[configgy]  lazy val prefix = {
        if(full contains "."){
        	full.substring(0, full.lastIndexOf('.')+1)
        }else{
            ""
        }
      }

      protected[configgy]  lazy val name = {
        if(full contains "."){
        	full.substring(full.lastIndexOf('.')+1)
	    }else{
	    	full
	    }
      }
   	  protected[configgy] val comparisonKey : String = {
   			val exception = new Exception()
   			val trace = exception.getStackTrace
	     	val skipThisFileName = trace(0).getFileName 
		 		val skipped = trace.dropWhile(_.getFileName == skipThisFileName)
		 		val lineNumberString = ""+skipped(0).getLineNumber
		 		val padding = "0000000000".drop(lineNumberString.length)
		 		""+skipped(0).getFileName+":"+padding + lineNumberString +" -> "+skipped(0)
   	  } 
      override def compare(that:SelfNaming) : Int = comparisonKey.compareTo(that.comparisonKey)
      protected[configgy] def write(sb : scala.StringBuilder, indent : String, inPrefix:String)=()
	}

	sealed protected trait Holder extends SelfNaming with ConfigurationSchema.Selfdocumenting {
		  protected var members : List[ConfigurationSchema.Member] = Nil
		  def map[B](func:ConfigurationSchema.Member=>B) = {
		    initMembers
		    members.map(func)
      }
    
		  protected[configgy] lazy val initMembers = {
			  // reflection magic to make sure all fields are initialized and set up holder in members
		    
			  // identify the class that has the members
		   	val cls = if(Holder.this.isInstanceOf[ConfigurationSchema]){
		   		var c = Holder.this.getClass
				  var last = c
				  while(c!=null && c!=classOf[ConfigurationSchema]) {
				   	last=c
				   	c=c.getSuperclass
	//println("moving up from "+last.getSimpleName+" to "+ c.getSimpleName)           
					}
				  last
        } else Holder.this.getClass

		    val found = for(m <- cls.getDeclaredMethods){
		        val typ = m.getReturnType
		       if( classOf[ConfigurationSchema.Member].isAssignableFrom(m.getReturnType) && m.getReturnType.getSimpleName.endsWith(m.getName+"$" )){
//println(this.getClass.getSimpleName+" holds "+ m.getReturnType.getSimpleName)	         
		           try{
		       		val member = m.invoke(Holder.this).asInstanceOf[ConfigurationSchema.Member]
		       		members = member :: members
		           }catch{case _ => }
		       }
//else println(this.getClass.getSimpleName+" does not hold "+ m.getReturnType.getSimpleName + " in "+m.getName)
		    }
//println(this.getClass.getSimpleName+" does not hold "+ m.getReturnType.getSimpleName + " in "+m.getName)        
			  members = members.sort(_ < _)
	    
		      true
		  }  
	   	  protected[configgy] def readConfiggy(in : Config):Unit= {
		    initMembers
		  	for(member<-members) member readConfiggy in
		  }  
	      override protected[configgy] def write(sb : scala.StringBuilder, indent : String, inPrefix:String){
		    initMembers
		    writeDocumentation(sb, indent, prefix)
		    val blanks = changeGroup(sb, inPrefix, full)
		    for(group<-members) group.write(sb, blanks, full)
		    changeGroup(sb, inPrefix, full)
		  }
	      private def changeGroup(sb : StringBuilder, oldP : String, newP : String):String = {
			  var oldS = List.fromString(oldP, '.')
			  var newS = List.fromString(newP, '.')
			  var count = -1
	//println("changegroup "+oldP+" -> "+newP + " \n   in "+full)    
			  while (newS != Nil && oldS!=Nil &&  oldS.head == newS.head ){
			    newS = newS.tail
			    oldS = oldS.tail
			    count+=1
			  }
			  count+=oldS.length
			  for(close <- oldS.reverse){
			    sb append ("   "*count)+"</"+close+">\r\n"
			    count-=1
			  }
			  for(open<- newS){
			    count+=1
			    sb append ("   "*count)+"<"+open+">\r\n"
			  }
			  "   "*(1+count)
			} 
	
	}

//  sealed protected[configgy] class DocMember( val v : String ) extends Member{
//	  override def readConfiggy(in:Config) = ()
//	  override def write(sb : scala.StringBuilder, indent : String, prefix : String){
//	    documentation foreach (_.write(sb, indent, prefix))
//		def comment(line:String) = sb.append(indent.drop(2)+"# "+line+"\r\n")
//        sb.append(indent.drop(1)+"###\r\n")
//	    for(line <- v.lines) comment (line)
//	  }  
//	}

object ConfigurationSchema {  
	   implicit def fieldReadConversionString (in : ConfigurationSchema#Field[String]) : String = in.apply
	   implicit def fieldReadConversionBoolean (in : ConfigurationSchema#Field[Boolean]) : Boolean = in.apply
	   implicit def fieldReadConversionInt (in : ConfigurationSchema#Field[Int]) : Int = in.apply
   	
    
    	sealed trait Member extends SelfNaming with Selfdocumenting  {
	    protected[configgy] def readConfiggy(in:Config)
//	    protected[configgy] lazy val documentation : Option[DocMember]= None
	    /**
         * override with a string to create documentation, may be multiline  
         */
//	    protected def doc : String = null
	} 

	/**
   * documentation members are var not val, but that is more than acceptable 
   * (might even be nice sometimes, but documentation content from files is never _read_)
   * 
   * the upside is that definitions can simply say 'doc = "bla"'
   */
	sealed protected[configgy] trait Selfdocumenting {
	  def documentationString = doc
   
	  protected var doc = "" 
	  protected def writeDocumentation(sb : scala.StringBuilder, indent : String, prefix : String){
			def comment(line:String) = sb.append(indent+"# "+line+"\r\n")
	    if(doc!=null && !doc.trim.isEmpty){
        sb.append(indent+" ###\r\n")
        for(line <- doc.lines) comment (line)
      }
	  }
	}
}

