package de.immaterialien.sturmonanny.util

import _root_.net.lag.configgy

/**
 * a trait to mix into configuration variable holder objects for easy updatability
 * 
 * update with myObject(configgyConfiguration)
 */
trait ConfiggyGroup {
  /**
   * "implement" by copying var definitions and applying this regex replacement:
   * 
   *   = conf get "" or 
   * ->
   * $1 = conf get "$1" or $1
   * 
   */
  protected def update(in : Reconfigure):Unit

  class Reconfigure(conf : configgy.Config){
    class Field(field:String){
      def or(default:Int):Int = conf(name(field), default)
      def or(default:String):String = conf(name(field), default)
      def or(default:Boolean):Boolean = conf(name(field), default)
    }
    def get(field:String) = new Field(field)
  }

  private lazy val prefix = {
    val full = this.getClass.getSimpleName()
    full.replace("$", ".")
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
  final def apply(in : configgy.Config):Unit= update(new Reconfigure(in))
}
object ConfiggyConfigured {
	   protected val objectNamePattern = """^(?:.*\$)([^\$])\$$""".r
}