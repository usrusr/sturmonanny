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
    private var groups : List[Group] = Nil
    try{
 		this(configgy.Config.fromFile(file))
	}catch{
	  case x: _root_.java.io.IOException => x.printStackTrace
	  case x => println("failed to read configuration file '"+file+"': "+x)
	}
    
	def apply(conf : configgy.Config) = {
	  initFile
	  groups map (_ update conf)
	}

	override def toString = {
	  initFile
	  val sb = new scala.StringBuilder
      var prefix : String= ""
	  for(group<-groups){
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
		  groups = groups.sort((e1, e2) => (e1 compareTo e2) < 0)
	  true
	}
	initFile
 
	
	protected trait Group extends SortableByInitialisationStack{  
	  lazy val prefix = {
	    val full = this.getClass.getSimpleName()
	    full.replace("$", ".")
	  }
	
	  lazy val initGroup = {
		// reflection magic to make sure all fields are initialized
	      val array = Group.this.getClass.getDeclaredMethods
	      val list = array.toList
	      val found = array foreach {m=>
	       if( m.getReturnType.getSimpleName.endsWith(m.getName+"$" )){
	           try{
	       		m.invoke(Group.this)
	           }catch{case _ => }
	       }
	      }
		  members = members.sort((a,b)=>(a compareTo b)<0)
    
		  ConfiggyFile.this.groups = ConfiggyFile.this.groups ::: Group.this :: Nil  
	      true
	  }
	  def write(sb : scala.StringBuilder, indent : String){
	    initGroup
	    for(member <- members) {
	      member.write(sb, indent)

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
	  	for(member<-members) member readConfiggy in
	  }  

    protected trait Member extends SortableByInitialisationStack{
	    members = members ::: this :: Nil      
        lazy val name : String= { 
	      val group = Member.this
	      	Member.this.getClass.getSimpleName match {
	      	  case ConfiggyConfigured.extractFieldName(name) => name
	      	  case _ => ""
	      	}
	    }
	    lazy val pathname : String= {
	      val group = Member.this
	      	Member.this.getClass.getSimpleName match {
	      	  case ConfiggyConfigured.extractFieldName(name) => name
	      	  case _ => ""
	      	}
	    }
	    protected[Group] def readConfiggy(in:configgy.Config)
	    def write(sb : scala.StringBuilder, indent : String)
    }
    /**
     * key/value pairs of a certain type that are on the same level as Field
     */
	protected[ConfiggyFile] class Table[T]( var v : T ) extends Member{
	  val defaultValue = v
	  var map = Map[String, T]()
	  def apply(what:String) : T = map.get(what) getOrElse defaultValue
	  def update(what:String, value:T) = map = map + ((what, value))
	  val extractor = v match {
     	case x:Int => (cMap : configgy.ConfigMap, k:String, oldValue:T)=>cMap(k,oldValue.asInstanceOf[Int]).asInstanceOf[T]
     	case x:String => (cMap : configgy.ConfigMap, k:String, oldValue:T)=>cMap(k,oldValue.asInstanceOf[String]).asInstanceOf[T]
     	case x:Boolean => (cMap : configgy.ConfigMap, k:String, oldValue:T)=>cMap(k,oldValue.asInstanceOf[Boolean]).asInstanceOf[T]
     
	    case _ => (cMap : configgy.ConfigMap, k:String, oldValue:T)=>{
	      oldValue
	    }
	  } 
      def printer = defaultValue match {
	    case _:String=> (what:T)=>{"\""+what+"\""}
	    case _ => (what:T)=>{""+what}
	  }
	  protected[Group] override def readConfiggy(in:configgy.Config){
	  	    in.getConfigMap(Group.this.name(name)) foreach {cMap =>
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
	    def write(sb : scala.StringBuilder, indent : String){     
		  sb.append(indent+"<"+ name+">\r\n")
		  for((k,v)<-map.projection) sb.append(indent+"   "+k+" = "+printer(v) +"\r\n")
		  sb.append(indent+"</"+ name+">\r\n")
        }
	} 
  	protected[ConfiggyFile] class Field[T]( var v : T ) extends Member{
	    def update(t:T)={v = t}
	    def apply = v

	
	    protected[Group] override def readConfiggy(in:configgy.Config){
	    	v match{
			      case x : String => v = in(Group.this.name(name), x).asInstanceOf[T]  
			      case x : Int => v = in(Group.this.name(name), x).asInstanceOf[T]  
			      case x : Boolean => v = in(Group.this.name(name), x).asInstanceOf[T]  
	    	}
	    }

	    override def toString = ""+v
	    def write(sb : scala.StringBuilder, indent : String){     
	      def string = v match {
		    case x:String=> "\""+x+"\""
		    case x => x
		  }
		  sb.append(indent+ name+"="+string+"\r\n")
        }
	  }
	  private var members : List[ Member ] = Nil
   }
 
	/**
	 * ordering helper based on a little reflective magic stack-trace magic, needs stack traces to work!
	 */
	protected trait SortableByInitialisationStack extends Comparable[SortableByInitialisationStack]{
	   val comparisonKey : String = {
	     val exception = new Exception()
		 val trace = exception.getStackTrace
	     val skipThisFileName = trace(0).getFileName 
		 val skipped = trace.dropWhile(_.getFileName == skipThisFileName)
		 val result = ""+skipped(0).getFileName+":"+skipped(0).getLineNumber +" -> "+skipped(0)
debug(result)
		 result
	   }
       override def compareTo(that:SortableByInitialisationStack) : Int = comparisonKey.compareTo(that.comparisonKey)
	}
}
object ConfiggyFile {
   implicit def fieldReadConversionString (in : ConfiggyFile#Group#Field[String]) : String = in.apply
   implicit def fieldReadConversionBoolean (in : ConfiggyFile#Group#Field[Boolean]) : Boolean = in.apply
   implicit def fieldReadConversionInt (in : ConfiggyFile#Group#Field[Int]) : Int = in.apply

}
