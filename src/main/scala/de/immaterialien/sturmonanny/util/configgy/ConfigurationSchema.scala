package de.immaterialien.sturmonanny.util.configgy

import net.lag.configgy.{ Config, ConfigMap }
import _root_.de.immaterialien.sturmonanny.util._

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
 *	  object hostExample extends Field( "127.0.0") 
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

abstract class ConfigurationSchema(val file: String) extends Holder with ConfigurationSchema.Selfdocumenting with Log {
  val fileReference = this({
    log.trace("parsing file " + file)
    if (file == null) new Config
    else {
    	val f = new java.io.File(file)
    	Config.fromFile(f.getAbsolutePath)
//      Config.fromFile(file)
    }
  })
  //	if(logging.isTraceEnabled){
  if (fileReference.isDefined) {
    log.debug("======\nraw file " + file + ":\n" + scala.io.Source.fromFile(fileReference.get.getAbsoluteFile).mkString)
  }
  log.debug("--->\nresult for " + file + ":\n" + this.toString)

  //	} 

  def writeToFilesystemOrMessage(): Option[String] = {
    fileReference match {
      case None => Some("transient configuration")
      case Some(f) => writeToFilesystemOrMessage(f)
    }
  }
  def writeToFilesystemOrMessage(f: java.io.File): Option[String] = try {
    val w = new java.io.FileWriter(f)
    w.write(this.toString)
    w.close
    None
  } catch {
    case e: java.io.IOException => Some("failed to write "+f.getAbsolutePath+" "+e.getMessage)
  }

  object status {
    private var internalMessages: List[ConfigurationSchema.Msg] = Nil
    def messages = internalMessages
    def clear = internalMessages = Nil
    def warning(msg: String) = internalMessages :::= ConfigurationSchema.Warning(msg) :: Nil
    def error(msg: String) = internalMessages :::= ConfigurationSchema.Error(msg) :: Nil
    def success(msg: String) = internalMessages :::= ConfigurationSchema.Success(msg) :: Nil
  }
  def apply(conf: Config): Option[java.io.File] = {
    status.clear

    initMembers
    readConfiggy(conf)
    if (file != null && conf.importer != null && conf.importer.isInstanceOf[net.lag.configgy.FilesystemImporter]) {
      val fsImporter = conf.importer.asInstanceOf[net.lag.configgy.FilesystemImporter]
      if (fsImporter.baseFolder != null) Some(new java.io.File(fsImporter.baseFolder, file))
      else None
    } else None
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
  protected[configgy] trait Group extends Holder with ConfigurationSchema.Member {
    override def toString = "group " + configgyName
  }

  /**
   * key/value pairs of a certain type, we don't just dive into the configgy ConfigMap because we want 
   * the types, the default values and the ability to merge a new Config into an existing ConfigFile
   */
  protected[configgy] class Table[T](var v: T) extends ConfigurationSchema.Member with ValidationInfo {
    val defaultValue = v
    var map = Map[String, T]()
    def apply(what: String): T = map.get(what) getOrElse defaultValue
    def update(what: String, value: T) = map = map + ((what, value))
    def toList: List[T] = {
      val it = map.values
      it.toList
    }
    val extractor = v match {
      case x: Int => (cMap: ConfigMap, k: String, oldValue: T) => cMap(k, oldValue.asInstanceOf[Int]).asInstanceOf[T]
      case x: String => (cMap: ConfigMap, k: String, oldValue: T) => cMap(k, oldValue.asInstanceOf[String]).asInstanceOf[T]
      case x: Boolean => (cMap: ConfigMap, k: String, oldValue: T) => cMap(k, oldValue.asInstanceOf[Boolean]).asInstanceOf[T]

      case _ => (cMap: ConfigMap, k: String, oldValue: T) => {
        oldValue
      }
    }

    def printer = defaultValue match {
      case _: String => (what: T) => { "\"" + what + "\"" }
      case _ => (what: T) => { "" + what }
    }
    override def readConfiggy(in: Config) {
      in.getConfigMap(full) foreach { cMap =>
        var nMap = map
        nMap = nMap.filter(cMap.keys.contains(_))
        for (k <- cMap.keys) {
          val oldV: Option[T] = Table.this.map.get(k)
          if (oldV.isDefined) {
            val newV = extractor(cMap, k, oldV.get)
            if (newV != oldV.get) {
              nMap = nMap + ((k, newV))
            }
          } else {
            nMap = nMap + ((k, extractor(cMap, k, defaultValue)))
          }
        }
        map = nMap
      }

    }
    override def write(sb: scala.StringBuilder, indent: String, prefix: String) {
      writeDocumentation(sb, indent, full)

      if( ! configgyName.isEmpty) sb.append(indent + "<" + configgyName + ">\r\n")
      innerTable(sb, indent)
      if( ! configgyName.isEmpty) sb.append(indent + "</" + configgyName + ">\r\n")
    }
    override def toString = "table " + configgyName + ":" + map
    def innerTable(sb: scala.StringBuilder, indent: String) = for ((k, v) <- map.projection) sb.append(indent + "   " + k + " = " + printer(v) + "\r\n")
    def innerTable: String = {
      val sb = new scala.StringBuilder()
      innerTable(sb, "")
      sb.toString
    }
  }

  protected[configgy] class Field[T](var v: T) extends ConfigurationSchema.Member with ValidationInfo {
    def update(t: T) = { if (t != null) v = t }
    def apply = v

    override def readConfiggy(in: Config) = v match {
      case x: String => Field.this() = in(full, x).asInstanceOf[T]
      case x: Int => Field.this() = in(full, x).asInstanceOf[T]
      case x: Boolean => Field.this() = in(full, x).asInstanceOf[T]
    }

    override def toString = v.toString
    override def write(sb: scala.StringBuilder, indent: String, prefix: String) {
      writeDocumentation(sb, indent, full)
      def string = v match {
        case x: String => "\"" + x + "\""
        case x => x
      }
      sb.append(indent + configgyName + "=" + string + "\r\n")
    }

  }

  /**
   * some nullable fields 
   */
  protected[configgy] trait ValidationInfo {
    /**
     * applies to string  
     * plain nullable is easier to setup/define
     * (only enforced in LiftSupport)
     */
    private var _pattern: scala.util.matching.Regex = null
    protected[ValidationInfo] def pattern_=(in: scala.util.matching.Regex) = _pattern = in
    protected[configgy] def pattern = _pattern
    /**
     * applies to string 
     * (only enforced in LiftSupport)
     */
    private var _maxLength: java.lang.Integer = null
    protected[ValidationInfo] def maxLength_=(in: Int) = _maxLength = new java.lang.Integer(in)
    protected[configgy] def maxLength = _maxLength

    /**
     * applies to int 
     * (only enforced in LiftSupport)
     */
    private var _min: java.lang.Integer = null
    protected[ValidationInfo] def min_=(in: Int) = _min = new java.lang.Integer(in)
    protected[configgy] def min = _min

    /**
     * applies to int 
     * (only enforced in LiftSupport)
     */
    private var _max: java.lang.Integer = null
    protected[ValidationInfo] def max_=(in: Int) = _max = new java.lang.Integer(in)
    protected[configgy] def max = _max
  }
}

/**
 * ordering helper based on a little reflective magic stack-trace magic, needs stack traces to work!
 */

sealed trait SelfNaming extends Ordered[SelfNaming] {
  protected def fullNameFromClass(cls: java.lang.Class[_]) = {
    val tmp = cls.getSimpleName()
    val dotsTmp = tmp.replace("$", ".")
    if (dotsTmp contains ".") {
      dotsTmp.substring(0, dotsTmp.length - 1)
    } else {
      dotsTmp
    }
  }
  protected def nameGivingClass: java.lang.Class[_] = this.getClass
  protected[configgy] lazy val full = {
    fullNameFromClass(nameGivingClass)
  }
  protected[configgy] lazy val prefix = {
    if (full contains ".") {
      full.substring(0, full.lastIndexOf('.') + 1)
    } else {
      ""
    }
  }

  protected[configgy] lazy val configgyName = {
    if (full contains ".") {
      full.substring(full.lastIndexOf('.') + 1)
    } else {
      full
    }
  }
  protected[configgy] val comparisonKey: String = {
    val exception = new Exception()
    val trace = exception.getStackTrace
    val skipThisFileName = trace(0).getFileName
    val skipped = trace.dropWhile(_.getFileName == skipThisFileName)
    val lineNumberString = "" + skipped(0).getLineNumber
    val padding = "0000000000".drop(lineNumberString.length)
    "" + skipped(0).getFileName + ":" + padding + lineNumberString + " -> " + skipped(0)
  }
  override def compare(that: SelfNaming): Int = comparisonKey.compareTo(that.comparisonKey)
  protected[configgy] def write(sb: scala.StringBuilder, indent: String, inPrefix: String) = ()
}

sealed protected trait Holder extends SelfNaming with ConfigurationSchema.Selfdocumenting {
  protected var members: List[ConfigurationSchema.Member] = Nil
  def map[B](func: ConfigurationSchema.Member => B) = {
    initMembers
    members.map(func)
  }
  private lazy val processedNameGivingClass = {
    if (Holder.this.isInstanceOf[ConfigurationSchema]) {
      var c = Holder.this.getClass
      var last = c
      while (c != null && c != classOf[ConfigurationSchema]) {
        last = c
        c = c.getSuperclass
      }
      last
    } else Holder.this.getClass
  }
  protected override def nameGivingClass: java.lang.Class[_] = processedNameGivingClass

  protected[configgy] lazy val initMembers = {
    // reflection magic to make sure all fields are initialized and set up holder in members
    val found = for (m <- nameGivingClass.getDeclaredMethods) {
      val typ = m.getReturnType
      if (classOf[ConfigurationSchema.Member].isAssignableFrom(m.getReturnType) && m.getReturnType.getSimpleName.endsWith(m.getName + "$")) {
        try {
          val member = m.invoke(Holder.this).asInstanceOf[ConfigurationSchema.Member]
          members = member :: members
        } catch { case _ => }
      }
    }
    members = members.sortWith(_ < _)
    true
  }
  protected[configgy] def readConfiggy(in: Config): Unit = {
    initMembers
    for (member <- members) member readConfiggy in
  }
  override protected[configgy] def write(sb: scala.StringBuilder, indent: String, inPrefix: String) {
    initMembers
    writeDocumentation(sb, indent, full)
    val blanks = changeGroup(sb, inPrefix, full)
    for (group <- members) group.write(sb, blanks, full)
    changeGroup(sb, full, inPrefix)
  }
  private def changeGroup(sb: StringBuilder, oldP: String, newP: String): String = {
    var oldS = oldP.split('.').toList
    var newS = newP.split('.').toList
    var count = -1
    while (newS != Nil && oldS != Nil && oldS.head == newS.head) {
      newS = newS.tail
      oldS = oldS.tail
      count += 1
    }
    count += oldS.length
    for (close <- oldS.reverse) {
      if( ! close.isEmpty) sb append ("   " * count) + "</" + close + ">\r\n"
      count -= 1
    }
    for (open <- newS) {
      count += 1
      if( ! open.isEmpty) sb append ("   " * count) + "<" + open + ">\r\n"
    }
    "   " * (1 + count)
  }

}

object ConfigurationSchema {
  implicit def fieldReadConversionString(in: ConfigurationSchema#Field[String]): String = in.apply
  implicit def fieldReadConversionBoolean(in: ConfigurationSchema#Field[Boolean]): Boolean = in.apply
  implicit def fieldReadConversionInt(in: ConfigurationSchema#Field[Int]): Int = in.apply

  sealed trait Member extends SelfNaming with Selfdocumenting {
    protected[configgy] def readConfiggy(in: Config)
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

    protected def writeDocumentation(sb: scala.StringBuilder, indent: String, full: String) {
      val prefix = full.replace(".", "/")
      def comment(line: String) = sb.append(
        " " +
        indent + "# " + line + "\r\n"
        )
      if (doc != null && !doc.trim.isEmpty) {
        sb.append(
          " " +
          //          "\r\n"+
          indent + " ### "
          + prefix
          //        +":"
          + " ###"
          + "\r\n")
        for (line <- doc.lines) comment(line)
      }
    }
  }
  abstract class Msg(val msg: String)
  case class Warning(override val msg: String) extends Msg(msg)
  case class Error(override val msg: String) extends Msg(msg)
  case class Success(override val msg: String) extends Msg(msg)
}

