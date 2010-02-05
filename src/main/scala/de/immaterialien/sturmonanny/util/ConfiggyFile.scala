package de.immaterialien.sturmonanny.util


import net.lag.configgy

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
abstract class ConfiggyFile(val file : String) extends Logging{ 
    private var all : List[Group] = Nil
    try{
 		this(configgy.Config.fromFile(file))
	}catch{
	  case x: _root_.java.io.IOException => x.printStackTrace
	  case x => println("failed to read configuration file '"+file+"': "+x)
	}
    
	def apply(conf : configgy.Config) = {
	  initFile
	  all map (_ update conf)
	}

	override def toString = {
	  initFile
	  val sb = new scala.StringBuilder
      var prefix : String= ""
	  for(group<-all){
	    val blanks = changeGroup(sb, prefix, group.prefix)
	    group.write(sb, blanks)
	    prefix = group.prefix
	  }
	  changeGroup(sb, prefix, "")
	  sb toString
	}
	private def changeGroup(sb : StringBuilder, oldP : String, newP : String):String = {
	  var oldS = List.fromString(oldP, '.')
	  var newS = List.fromString(newP, '.')
	  var count = -1
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
	lazy val initFile : Boolean = {
		// reflection magic to make sure all fields are initialized
	      val array = ConfiggyFile.this.getClass.getDeclaredMethods
	      val members = List.fromArray(array)
	      val found = members foreach {m=>
	       if( m.getReturnType.getSimpleName.endsWith(m.getName+"$" )){
	           try{
	       		m.invoke(ConfiggyFile.this) . asInstanceOf[Group] . initGroup
	           }catch{case _ => }
	       }
	      }	  
	  true
	}
	initFile
 
	protected trait Group {  
	  lazy val prefix = {
	    val full = this.getClass.getSimpleName()
	    full.replace("$", ".")
	  }
	
	  lazy val initGroup = {
		// reflection magic to make sure all fields are initialized
	      val array = Group.this.getClass.getDeclaredMethods
	      val members = List.fromArray(array)
	      val found = members foreach {m=>
	       if( m.getReturnType.getSimpleName.endsWith(m.getName+"$" )){
	           try{
	       		m.invoke(Group.this)
	           }catch{case _ => }
	       }
	      }
		  ConfiggyFile.this.all = ConfiggyFile.this.all ::: Group.this :: Nil  
	      true
	  }
	  def write(sb : scala.StringBuilder, indent : String){
	    initGroup
	      for(f <- fields) {
	    	  var v = f.apply match {
	    	    case x:String=> "\""+x+"\""
	    	    case x => x
	    	  }
	    	  
		      sb.append(indent+ f.name+"="+v+"\r\n")
	      }
	  }
	  
	  private def name(prop:String) : String = prefix+prop
	  private def set(default : Int)(conf:configgy.Config)(field:String):Int = {
		 conf.apply(name+"."+field, default)
	  }
	  private def set(default : Boolean)(conf:configgy.Config)(field:String):Boolean = {
		 conf.apply(name+"."+field, default)
	  }
	  private def set(default : String)(conf:configgy.Config)(field:String):String = {
		 conf.apply(name+"."+field, default)
	  }
	  private def name = "" 
	  final def update(in : configgy.Config):Unit= {
	    initGroup
	  	for(f<-fields){
		    f.apply match{
		      case v : String => f.asInstanceOf[Field[String]].update(in(name(f.name), v)) 
		      case v : Int => f.asInstanceOf[Field[Int]].update(in(name(f.name), v)) 
		      case v : Boolean => f.asInstanceOf[Field[Boolean]].update(in(name(f.name), v))
		      case x => println("unknown:"+x)
		    }
	     }
	  }  

  
  	protected[ConfiggyFile] class Field[T]( var v : T ){
	    def update(t:T)={v = t}
	    def apply = v
	    fields = fields ::: this :: Nil
	
	    
	    lazy val name : String= {
	      val group = Field.this
	      	Field.this.getClass.getSimpleName match {
	      	  case ConfiggyConfigured.extractFieldName(name) => name
	      	  case _ => ""
	      	}
	    }
	    lazy val pathname : String= {
	      val group = Field.this
	      	Field.this.getClass.getSimpleName match {
	      	  case ConfiggyConfigured.extractFieldName(name) => name
	      	  case _ => ""
	      	}
	    }
	    override def toString = ""+v
	  }
	  private var fields : List[ Field[_] ] = Nil
	} 
}
object ConfiggyFile {
   implicit def fieldReadConversionString (in : ConfiggyFile#Group#Field[String]) : String = in.apply
   implicit def fieldReadConversionBoolean (in : ConfiggyFile#Group#Field[Boolean]) : Boolean = in.apply
   implicit def fieldReadConversionInt (in : ConfiggyFile#Group#Field[Int]) : Int = in.apply
}
