package de.immaterialien.sturmonanny.util

import _root_.net.lag.configgy

/**
 * a trait to mix into configuration variable holder objects for easy updatability
 * 
 * update with myObject(configgyConfiguration)
 */
trait ConfiggyGroup {

  protected def update(in : configgy.Config):Unit = {
	init
  }
  lazy val prefix = {
    val full = this.getClass.getSimpleName()
    full.replace("$", ".")
  }

    lazy val init = {
      val array = ConfiggyGroup.this.getClass.getDeclaredMethods
      val members = List.fromArray(array)
      val found = members foreach {m=>
       if( m.getReturnType.getSimpleName.endsWith(m.getName+"$" )){
           try{
       		m.invoke(ConfiggyGroup.this)
           }catch{case _ => }
       }
      }
      true
  }
  def write(sb : scala.StringBuilder, indent : String){
    init
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
  final def apply(in : configgy.Config):Unit= for(f<-fields){
    init
    f.apply match{
      case v : String => f.asInstanceOf[Field[String]].update(in(f.name, v)) 
      case v : Int => f.asInstanceOf[Field[Int]].update(in(f.name, v)) 
      case v : Boolean => f.asInstanceOf[Field[Boolean]].update(in(f.name, v))
      case x => println("unknown:"+x)
    }
  }  

  
  class Field[T]( var v : T ){
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
//    implicit def fToT(f : ConfiggyGroup#Field[T]) : T = v
    override def toString = ""+v
  }
  private var fields : List[ Field[_] ] = Nil
}

object ConfiggyConfigured {
//	protected val objectNamePattern = """^(?:.*\$)([^\$]*)\$$""".r
    val extractFieldName = """.*\$([^\$]+)\$$""".r 
}