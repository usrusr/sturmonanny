package de.immaterialien.sturmonanny.dcg

import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._

class AllPlanesEverywhere(args: String) extends javax.xml.ws.Provider[File] with Log {
  override def invoke(file: File): File = {

    val g = new AllPlanesEverywhere.Gatherer
    val f1 = new FileReader(file)
    val pr1 = g.parseAll(g.fileParser, f1)
    f1.close

    {
      var i = 0
      for (side <- g.dromesToSides) {
        val set = g.sideToPlanes.get(side).getOrElse {
          val s = new mutable.LinkedHashSet[String]
          g.sideToPlanes.put(side, s)
          s
        }
        val planes = g.dromesToPlanes(i)
        i += 1
        set ++= planes
      }
    }
    
    println("between " + pr1 +
//      "\ndromesToSides = " + g.dromesToSides +
//      "\ndromesToPlanes = " + g.dromesToPlanes +
//      "\nsideToPlanes = " + g.sideToPlanes +
      "")

    log debug "" + pr1
    val out = new File(file.getParent, file.getName + ".flat.mis")
    val w = new FileWriter(out)
    g.writer = Some(w)
    val f2 = new FileReader(file)
    val pr2 = g.parseAll(g.fileParser, f2)
    f2.close
    w.close
    log debug "" + pr2
    out
  }
}

object AllPlanesEverywhere extends Log {
  /**
   * will make two passes: 
   * first with writer = None gathering info in the two maps
   * second with writer = Some(x) 
   * third with writer , gathering in dromesToSides 
   * @author ulf
   */
  protected class Gatherer extends RegexParsers {
    case object kept extends Kept
    trait Kept
    implicit def multipleKepts(ks: Seq[Kept]): Kept = kept
    implicit def seqKepts(k1: ~[Kept, Kept]): Kept = kept

    override def skipWhitespace = false
    val dromesToSides = new mutable.ArrayBuffer[Int]
    //    val dromesToPlanes = new mutable.HashMap[Int, mutable.Set[String]]
    val dromesToPlanes = new mutable.ArrayBuffer[List[String]]
    /**
     * will be populated between first and second pass 
     */
    val sideToPlanes = new mutable.HashMap[Int, mutable.Set[String]]
    var writer: Option[Writer] = None

    def pass[T](in: T): T = {
      writer map (_ append in.toString)
      in
    }
    def keep(in: String): Kept = {
      writer map (_ append in)
      kept
    }
    lazy val fileParser: Parser[Kept] = {
      rep((iniLine("BornPlace") ~> bornPlaces) | (iniLine ~ rep(anyLine)) | "\\z".r
        //) ~ "$".r  ^^^ kept
		) ~ (o ~ "\\z?".r) ^^^kept
    }

    def iniLine(what: String): Parser[Kept] = {
      o ~> ("[" ~> what <~ "]") ^^ (s => keep("[" + s + "]"))
    }
    def k(what: String): Parser[Kept] = {
      what ^^ keep
    }

    lazy val stringLine: Parser[String] = ("\r\n" | "\n\r" | "\n")
    lazy val nl: Parser[Kept] = o ~> (stringLine ^^ keep)
    //    lazy val nl: Parser[Kept] = w

    lazy val iniLine: Parser[Kept] = {
      o ~> ("[" ~> "[^\\]]+".r <~ "]") ^^ (s => keep("[" + s + "]"))
    }
    lazy val anyLine: Parser[Kept] = {
      o ~> ("[^\\[].*".r ^^ keep)
    }
    lazy val bornPlaces: Parser[Kept] = {
      rep(bornPlace) ~
        rep(o ~> aerodrome) ^^^
        kept
    }
    lazy val aerodrome: Parser[Kept] = {
      (("[BornPlace" ~> int <~ "]") ~
        rep("""\s*[^\[\s]\S*""".r )) ^^ {
        case number ~ planes => {
          writer map { w =>
            w append ("[BornPlace" + number + "]")
            //            
            val side = dromesToSides(number)
            val sp = sideToPlanes.get(side)
            for (set <- sp; p <- set) w append ("  " + p)
            kept
          } getOrElse {
            //            val planeSet = dromesToPlanes.get(number).getOrElse {
            //              val set = new mutable.HashSet[String]
            //              dromesToPlanes put (number, set)
            //              set
            //            }
            //            planeSet ++= planes
            dromesToPlanes += planes
            kept
          }
        }
      }
    }
    lazy val bornPlace: Parser[Kept] = {
      (o ~> keepBornPlace ~ w ~ keepDouble ~ w ~ keepDouble ~ w ~ keepDouble) ^^^ kept
    }
    lazy val keepBornPlace: Parser[Kept] =
      int ^^
        { side =>
          if (writer isEmpty) {
            dromesToSides += side
            kept
          } else {
            keep(side.toString)
          }
        }
    lazy val int: Parser[Int] = {
      """-?\d+""".r ^^ (_.trim toInt)
    }
    lazy val keepDouble: Parser[Kept] = {
      """-?(\d+|(:?\d*\.\d+)?)""".r ^^ keep
    }
    lazy val double: Parser[Double] = {
      """-?(\d+|(:?\d*\.\d+)?)""".r ^^ (_.trim toDouble)
    }
    lazy val w: Parser[Kept] = whiteSpace ^^ keep
    lazy val o: Parser[Kept] = "\\s*".r ^^ keep
  }
}