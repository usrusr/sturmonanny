package de.immaterialien.qlmap;

import scala.collection.mutable
import java.io
import scala.util.parsing.combinator._
/**
 * reads a mapinfo
 * 
 * constructor argument: the content of the MAP line of a .mis
 *  
 * @author ulf
 *
 */
case class MapInfo(image: Option[io.File], width: Option[Double], height: Option[Double], widthOffset: Option[Double], heightOffset: Option[Double], errors: List[String]) {
  println("defining " + this)
  def and(other: MapInfo): MapInfo = {
    def nonnull[T](x: Option[T], y: Option[T]): Option[T] = if (x.isDefined && x.get != null) x else y
    MapInfo(nonnull(other.image, this.image),
      nonnull(other.width, this.width),
      nonnull(other.height, this.height),
      nonnull(other.widthOffset, this.widthOffset),
      nonnull(other.heightOffset, this.heightOffset),
      this.errors ::: other.errors)
  }
  def image(n: io.File): MapInfo = MapInfo(Some(n), width, height, widthOffset, heightOffset, errors)
  def width(n: Double): MapInfo = MapInfo(image, Some(n), height, widthOffset, heightOffset, errors)
  def height(n: Double): MapInfo = MapInfo(image, width, Some(n), widthOffset, heightOffset, errors)
  def widthOffset(n: Double): MapInfo = MapInfo(image, width, height, Some(n), heightOffset, errors)
  def heightOffset(n: Double): MapInfo = MapInfo(image, width, height, widthOffset, Some(n), errors)
  def errors(n: List[String]): MapInfo = MapInfo(image, width, height, widthOffset, heightOffset, errors ::: n)
}
object MapInfo extends MapInfo(None, None, None, None, None, Nil)
class MapBase(folder: io.File) {
  def this(baseFolder: String) = this(new io.File(baseFolder))
  class InfoParser(misFile: java.io.File) extends JavaTokenParsers {
    lazy val parseResult: MapInfo = {
      val file = new java.io.FileReader(misFile)
      val ret = this.parseAll(fileParser, file)
      file.close
      ret.get
    }
    private lazy val dbl: Parser[Double] = {
      """-?(\d+(\.\d*)?|\d*\.\d+)""".r ^^ (_ toDouble)
    }

    private lazy val fileParser: Parser[MapInfo] = {
      rep(("image" ~ "=" ~> """\S+""".r) ^^ { x => MapInfo.image(new io.File(folder, x)) } | ("width" ~ "=" ~> dbl) ^^ { x => MapInfo.width(x.toDouble) } | ("height" ~ "=" ~> dbl) ^^ { x => MapInfo.height(x.toDouble) } | ("widthOffset" ~ "=" ~> dbl) ^^ { x => MapInfo.widthOffset(x.toDouble) } | ("heightOffset" ~ "=" ~> dbl) ^^ { x => MapInfo.heightOffset(x.toDouble) } |
        ".+".r ^^ { x =>
          println("ignoring " + x)
          if (x.trim.isEmpty) MapInfo
          else MapInfo.errors(x :: Nil)
        }
        ) ^^ { x =>
        x.foldLeft(MapInfo.asInstanceOf[MapInfo]) { (acc, add) =>
          acc.and(add)
        }
      }
    }
  }

  val parsed = new mutable.HashMap[String, MapInfo]

  def info(mapString: String): Option[MapInfo] = {

    var cleanedString = mapString
    if (cleanedString endsWith MapBase.suffix) cleanedString = cleanedString.substring(0, cleanedString.length - MapBase.suffixLen)

    cleanedString = cleanedString.replaceAll("""\W""", "_")

    var ret = parsed.get(cleanedString)

    if (!ret.isDefined) {
      ret = Some(new InfoParser(new io.File(folder, cleanedString + MapBase.suffix)).parseResult)
    }

    ret
  }
  def all = {

    val list = folder.list(MapBase.dotMis)

    list map { mapinfo =>
      val id = mapinfo.substring(0, mapinfo.length - MapBase.suffixLen)
      info(id)
    }
  }
}
object MapBase {
  val suffix = ".mapinfo"
  val suffixLen = suffix.length
  val pat = (""".*\Q""" + suffix + """\E""").r
  object dotMis extends io.FilenameFilter {

    //    override def accept(path:io.File, fname:String):Boolean = fname match {
    //      case pat(_) => true
    //      case _ => false
    //    }
    override def accept(path: io.File, fname: String): Boolean = pat.unapplySeq(fname).isDefined

  }
}
