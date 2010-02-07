package de.immaterialien.sturmonanny.util.configgy

import net.lag.configgy.{Config, ConfigMap}

 
/**
 * base class for type-safe configurations consisting of a number of 
 * Group objects that are containing 
 * Field objects that are bound to a 
 * type (Int, Boolean, String) by providing an example value as constructor argument
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
}

 * 
 */
abstract class ConfigurationSchema(val file : String) extends Holder{ 
    
    try{
 		this(Config.fromFile(file))
	}catch{
	  case x: _root_.java.io.IOException => x.printStackTrace
	  case x => println("failed to read configuration file '"+file+"': "+x)
	}
    
	def apply(conf : Config) = {
	  initMembers
	 // members map (_ readConfiggy conf)
	  for(member<-members) member.readConfiggy(conf)
	}

	override def toString = {
	  initMembers
	  val sb = new scala.StringBuilder 

      write(sb, "", "")
	  sb toString
	}

 	initMembers

 	/**
 	 * Groups really are just Holders (of other groups or fields) and Members (of other Groups or ConfiggyFile) 
     */
	protected[ConfigurationSchema] trait Group extends Holder with Member {
	  override def toString = "group "+name
	}
 

 
	/**
     *  a helper to add documentation to the definition source file 
     *  that gets serialized by the ConfiggyFile
     */
	protected[ConfigurationSchema] class Documentation( var v : String ) extends Member{
	  override def readConfiggy(in:Config) = ()
	  override def write(sb : scala.StringBuilder, indent : String, prefix : String){
	    
		def comment(line:String) = sb.append(indent.drop(2)+"# "+line+"\r\n")
//		comment("=============")
        sb.append(indent.drop(1)+"###\r\n")
	    for(line <- v.lines) comment (line)
        //comment("")
	  }  
	}

    /**
     * key/value pairs of a certain type, we don't just dive into the configgy ConfigMap because we want 
     * the types, the default values and the ability to merge a new Config into an existing ConfigFile
     */
	protected[ConfigurationSchema] class Table[T]( var v : T ) extends Member{
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
	    def write(sb : scala.StringBuilder, indent : String, prefix : String){     
		  sb.append(indent+"<"+ name+">\r\n")
		  for((k,v)<-map.projection) sb.append(indent+"   "+k+" = "+printer(v) +"\r\n")
		  sb.append(indent+"</"+ name+">\r\n")
	    }
     override def toString = "table "+name+":"+map
	} 

    class Field[T]( var v : T ) extends Member{
	    def update(t:T)={v = t}
	    def apply = v
	
	    override def readConfiggy(in:Config) = v match{
		      case x : String => v = in(full, x).asInstanceOf[T]  
		      case x : Int => v = in(full, x).asInstanceOf[T]  
		      case x : Boolean => v = in(full, x).asInstanceOf[T]  
    	}
	
	    override def toString = "field "+name+":"+v
	    def write(sb : scala.StringBuilder, indent : String, prefix : String){     
	      def string = v match {
		    case x:String=> "\""+x+"\""
		    case x => x
		  }
		  sb.append(indent+ name+"="+string+"\r\n")
	    }
	  }



 }
 	
/**
 * ordering helper based on a little reflective magic stack-trace magic, needs stack traces to work!
 */

 	sealed trait SelfNaming extends Ordered[SelfNaming]{
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
		 ""+skipped(0).getFileName+":"+skipped(0).getLineNumber +" -> "+skipped(0)
   	  } 
      override def compare(that:SelfNaming) : Int = comparisonKey.compareTo(that.comparisonKey)
	}

	sealed protected trait Holder extends SelfNaming {
		  protected var members : List[Member] = Nil
		  protected[configgy] lazy val initMembers = {
			  // reflection magic to make sure all fields are initialized and set up holder in members
		      val array = Holder.this.getClass.getDeclaredMethods
		      val list = array.toList
		      val found = array foreach {m=>
		        val typ = m.getReturnType
		       if( classOf[Member].isAssignableFrom(m.getReturnType) && m.getReturnType.getSimpleName.endsWith(m.getName+"$" )){
	//debug(this.getClass.getSimpleName+" holds "+ m.getReturnType.getSimpleName)	         
		           try{
		       		val member = m.invoke(Holder.this).asInstanceOf[Member]
		       		members = member :: members
		           }catch{case _ => }
		       }
		      }
			  members = members.sort(_ < _)
	    
		      true
		  }  
	   	  protected[configgy] def readConfiggy(in : Config):Unit= {
		    initMembers
		  	for(member<-members) member readConfiggy in
		  }  
	      protected[configgy] def write(sb : scala.StringBuilder, indent : String, inPrefix:String){
		    initMembers
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
	sealed protected trait Member extends SelfNaming {
	    protected[configgy] def readConfiggy(in:Config)
	    protected[configgy] def write(sb : scala.StringBuilder, indent : String, prefix : String)
	} 

object ConfigurationSchema {  
	   implicit def fieldReadConversionString (in : ConfigurationSchema#Field[String]) : String = in.apply
	   implicit def fieldReadConversionBoolean (in : ConfigurationSchema#Field[Boolean]) : Boolean = in.apply
	   implicit def fieldReadConversionInt (in : ConfigurationSchema#Field[Int]) : Int = in.apply
   	
}

