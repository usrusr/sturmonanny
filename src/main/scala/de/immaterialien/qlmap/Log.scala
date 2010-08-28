package de.immaterialien.qlmap

trait Log {
  private val lg = try{
    val cls = this.getClass
    val lc = cls.getClassLoader.loadClass("org.apache.log4j.Logger")
    var meth = lc.getDeclaredMethod("createLogger", List(classOf[String]):_*) 
    Some(meth.invoke(null, List(lc.getCanonicalName):_*).asInstanceOf[{
      def warn(o:AnyRef, t:Throwable):Unit
      def error(o:AnyRef, t:Throwable):Unit
      def debug(o:AnyRef, t:Throwable):Unit
      def warn(o:AnyRef):Unit
      def error(o:AnyRef):Unit
      def debug(o:AnyRef):Unit
//      def isWarningEnabled:Boolean
      def isDebugEnabled():Boolean
//      def isErrorEnabled:Boolean
    }])
  }catch{case _=>None}
  
  val log = this.asInstanceOf[Log]
  
  /**
   * force a conversion to String before passing to an Object/AnyRef parameter
   * @param txt
   * @return
   */
  private def string(txt : =>String):String = txt
  
  def warn(txt : =>String)=lg.map(_.warn(string(txt))).getOrElse(println("warn: "+string(txt)))
  def warn(txt : =>String, t:Throwable)=lg.map(_.warn(string(txt), t)).getOrElse(println("warn: "+string(txt)+" <- "+t.getMessage+"\n"+t.getStackTraceString))
  
  def debug(txt : =>String)=lg.map(l=>if(l.isDebugEnabled)l.debug(string(txt))).getOrElse(println("debug: "+string(txt)))
  def debug(txt : =>String, t:Throwable)=lg.map(l=>if(l.isDebugEnabled)l.debug(string(txt), t)).getOrElse(println("debug: "+string(txt)+" <- "+t.getMessage+"\n"+t.getStackTraceString))

  def error(txt : =>String)=lg.map(_.error(string(txt))).getOrElse(println("error: "+string(txt)))
  def error(txt : =>String, t:Throwable)=lg.map(_.error(string(txt), t)).getOrElse(println("error: "+string(txt)+" <- "+t.getMessage+"\n"+t.getStackTraceString))

} 