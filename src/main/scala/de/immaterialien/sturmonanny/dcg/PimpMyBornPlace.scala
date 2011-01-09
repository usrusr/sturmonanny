package de.immaterialien.sturmonanny.dcg


import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._

class PimpMyBornPlace(args: String) extends DoNothingMisRewriter(args){
	override def gatherer = new PimpMyBornPlace.Gatherer(args)
//	override def intermediateName = "radar"
}

object PimpMyBornPlace {
  /**
   * will make one pass
   */
  protected class Gatherer(toAdd:String) extends DoNothingMisRewriter.Gatherer {
  	val internalToAdd = if(toAdd.startsWith("(") && toAdd.endsWith(")")) toAdd.drop(1).dropRight(1) else toAdd
  	
  	override lazy val bornPlace: Parser[Kept] = {
      (o ~> bornPlaceSide ~ w ~ keepDouble ~ w ~ keepDouble ~ w ~ keepDouble
      		~ (opt("[ \\t\\S]+".r ^^ keep)^^{ opt =>
      			if(opt.isEmpty) {
      				keep(internalToAdd)
      			} else {
      				// ignore bornplace if it already has something after the coords
      				kept
      			}
      		})
      		) ^^^ kept
    }
  }
}