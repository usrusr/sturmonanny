package de.immaterialien.sturmonanny.market.fixed


import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._


object AvailablePlanesIdentifier extends Log {
	def cycle(mis:java.io.File) =try{
		val gatherer = new MisPlanesSelector
		val reader = new java.io.FileReader(mis)
		val parseRet = gatherer.parseAll(gatherer.fileParser, reader)
		reader.close
		Map()++(gatherer.sideToPlanes.map{kv=> (kv._1 -> (Set[String]()++kv._2))})
	}catch{
		case x=>{
			log.error("could not identify planes from "+mis)
			Map[Int,scala.collection.mutable.Set[String]]()
		}
	}
	
  private class MisPlanesSelector extends de.immaterialien.sturmonanny.dcg.DoNothingMisRewriter.Gatherer {
    val dromesToSides = new mutable.ArrayBuffer[Int]
    val sideToPlanes = new mutable.HashMap[Int, mutable.Set[String]]
    override lazy val interestingBlocks: Parser[Kept] = {
      (iniLine("BornPlace") ~> bornPlaces) ^^^ kept
    }

    override lazy val aerodrome: Parser[Kept] = {
      (o ~> ("[BornPlace" ~> int <~ "]") ~
        rep(o ~> """[^\[\s]\S*""".r)) ^^ {
        case number ~ planes => {
          val side = dromesToSides(number)

          val planeset = sideToPlanes.get(side).getOrElse {
            val set: mutable.Set[String] = new mutable.HashSet()
            sideToPlanes += (side -> set)
            set
          }
          planeset ++= planes
          kept
        }
      }
    }

    override lazy val bornPlaceSide: Parser[Kept] =
      int ^^ { side =>
        if (writer isEmpty) {
          dromesToSides += side
          kept
        } else {
          keep(side.toString)
        }
      }
  }
}