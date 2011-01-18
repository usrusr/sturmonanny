package de.immaterialien.sturmonanny.dcg


import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._

//class AllPlanesEverywhere(args: String) extends javax.xml.ws.Provider[File] with Log {
class AllPlanesEverywhere(args: String) extends DoNothingMisRewriter(args){
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
        set ++= planes.map(_ trim)
      }
    }
    
    println("between " + pr1 +
//      "\ndromesToSides = " + g.dromesToSides +
//      "\ndromesToPlanes = " + g.dromesToPlanes +
//      "\nsideToPlanes = " + g.sideToPlanes +
      "")

    log debug "" + pr1
    val out = new File(file.getParent, file.getName.dropRight(4) + "."+intermediateName+".mis")
    if(out.exists) out.delete
    val w = new FileWriter(out)
    g.writer = Some(w)
    val f2 = new FileReader(file) 
    val pr2 = g.parseAll(g.fileParser, f2)
    f2.close
    w.close
    log debug "" + pr2
    out
    
    val bak = new File(file.getParent, file.getName + ".pre."+intermediateName)
    if(bak.exists) bak.delete
    file.renameTo(bak)
    out.renameTo(file)
    file
  }
}

object AllPlanesEverywhere {
  /**
   * will make two passes: 
   * first with writer = None gathering info in the two maps
   * second with writer = Some(x) 
   * third with writer , gathering in dromesToSides 
   * @author ulf
   */
  protected class Gatherer extends DoNothingMisRewriter.Gatherer {
    val dromesToSides = new mutable.ArrayBuffer[Int]
    val dromesToPlanes = new mutable.ArrayBuffer[List[String]]
    /**
     * will be populated between first and second pass 
     */
    val sideToPlanes = new mutable.HashMap[Int, mutable.Set[String]]
  	override lazy val interestingBlocks: Parser[Kept] = {
      		(iniLine("BornPlace") ~> bornPlaces) ^^^kept
    }
  	
    override lazy val aerodrome: Parser[Kept] = {
      (o~> ("[BornPlace" ~> int <~ "]") ~
        rep("""\s*[^\[\s]\S*""".r )) ^^ {
        case number ~ planes => {
          writer map { w =>
            w append ("\r\n[BornPlace" + number + "]\r\n")
            //            
            val side = dromesToSides(number)
            val sp = sideToPlanes.get(side)
            for (set <- sp; p <- set) w append ("  " + p+"\r\n")
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

    override lazy val bornPlaceSide: Parser[Kept] =
      int ^^
        { side =>
          if (writer isEmpty) {
            dromesToSides += side
            kept
          } else {
            keep(side.toString)
          }
        }
  }
}