package de.immaterialien.sturmonanny.dcg


import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._


/**
 * 
 * delay the first waypoint of chiefs, with configurable randomization
 * configuration: min=5 max=10
 * 
 * @author ulf
 *
 */
class DelayedChiefs (args: String) extends DoNothingMisRewriter(args){
	override def gatherer = new DelayedChiefs.Gatherer(range)
//	override def intermediateName = "delayedChiefs"
		
	lazy val range = DelayedChiefs.minMax(args)
}

object DelayedChiefs {
	val findMin = """(?i)min(?:Minutes)?\s*=\s*(\d+)""".r
	val findMax = """(?i)max(?:Minutes)?\s*=\s*(\d+)""".r
	def minMax(args: String):(Int, Int)={
		val min = findMin.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(0)
		val max = findMax.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(min)
		(min, max)
	}
	
  /**
   * will make one pass 
   */
  protected class Gatherer(range:(Int, Int)) extends DoNothingMisRewriter.Gatherer {
  	val min = math.min(range._1, range._2)
  	val max = math.max(range._1, range._2)
    override lazy val chiefRoads: Parser[Kept]  = (
    	("""\s*\[.*_Chief_Road\]""".r ^^ keep) ~ extendedChiefWaypoint ~ (opt(rep(chiefWaypoint))^^^kept) 
    )^^^kept
    
    lazy val extendedChiefWaypoint : Parser[Kept]  = (
    		o ~> keepDouble ~ w ~ keepDouble ~ w ~ keepDouble ~ w 
    		~ (int^^{old=>
    			val rand = min + (max-min).toDouble * math.random
    			val total = math.max(old, rand.toInt)
    			keep(total.toString)
    		})  
    		~ w ~ keepInt ~ w ~ keepDouble 
    )^^^kept  	
  }
}