package de.immaterialien.sturmonanny.dcg


import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._


/**
 * 
 * delay the first waypoint of chiefs, with configurable randomization
 * configuration: min=5 max=10 grid=500
 * <p>min and max are minutes, grid is in meters, and is optional (chiefs within the same grid will get the same delay) 
 * 
 * @author ulf
 *
 */
class DelayedChiefs (args: String) extends DoNothingMisRewriter(args){
	val range = DelayedChiefs.minMax(args)
	val grid = DelayedChiefs.grid(args)
	
	override def gatherer = new DelayedChiefs.Gatherer(range, grid)
}

object DelayedChiefs {
	val findMin = """(?i)min(?:Minutes)?\s*=\s*(\d+)""".r
	val findMax = """(?i)max(?:Minutes)?\s*=\s*(\d+)""".r
	val findGrid = """(?i)grid?\s*=\s*(\d+)""".r	
	def minMax(args: String):(Int, Int)={
		val min = findMin.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(0)
		val max = findMax.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(min)
		(min, max)
	}
	def grid(args: String):(Int)= findGrid.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(0)

  /**
   * will make one pass 
   */
  protected class Gatherer(range:(Int, Int), grid:Int) extends DoNothingMisRewriter.Gatherer {
  	val min = math.min(range._1, range._2)
  	val max = math.max(range._1, range._2)
  	lazy val seed = scala.util.Random.nextLong
  	lazy val reSeedable = new scala.util.Random
  	
  	override lazy val interestingBlocks: Parser[Kept] = {
      (iniLine("Chiefs") ~ rep(anyLine) ~> rep(chiefRoads)) ^^^  kept
    }
  	
  	
  	
    override lazy val chiefRoads: Parser[Kept]  = (
    	("""\s*\[.*_Chief_Road\]""".r ^^ keep) ~ extendedChiefWaypoint ~ (opt(rep(chiefWaypoint))^^^kept) 
    )^^^kept
    
    
    
//    lazy val extendedChiefWaypoint : Parser[Kept]  = (
//    		o ~> keepDouble ~ w ~ keepDouble ~ w ~ keepDouble ~ w 
//    		~ (int^^{old=>
//    			val rand = min + (max-min).toDouble * math.random
//    			val total = math.max(old, rand.toInt)
//    			keep(total.toString)
//    		})  
//    		~ w ~ keepInt ~ w ~ keepDouble 
//    )^^^kept  	
    
    lazy val extendedChiefWaypoint : Parser[Kept]  = (
//    		(o ~> doubleString ~ wString ~ doubleString ~ wString ~ doubleString ~ wString ~ int ^^ {
//    			case xs1 ~ w1 ~ ys1 ~ w2 ~ zs ~ w3 ~ rs ~ w4 ~ oldTime => {  
//	    			val rand = min + (max-min).toDouble * math.random
//	    			val total = math.max(old, rand.toInt)
//	    			
//	    			keep(xs1); keep(w1); keep(ys1); keep(w2); keep(zs); keep(w3); keep(rs); keep(w4); keep(total.toString) 
//	    			kept
//    			}
    		(o~> locationToWait) ~ w ~ keepInt ~ w ~ keepDouble 
    )^^^kept  
    lazy val locationToWait : Parser[Kept] = {
  		doubleString ~ wString ~ doubleString ~ wString ~ doubleString ~ wString ~ int ^^ {
    			case xs1 ~ w1      ~ ys1          ~ w2      ~ zs           ~ w3      ~ oldTime => {
    				
    				var rand = if(grid<=0) math.random else {
    					def bucketize(doubleString:String)= {
    						(doubleString.toDouble/grid).toLong 
    					}
    					reSeedable.setSeed((bucketize(xs1) ^ seed ^ (bucketize(ys1)<<8)))
    					reSeedable.nextDouble 
    				}
    				
	    			val transformedRand = min + (max-min).toDouble * rand
	    			val total = math.max(oldTime, transformedRand.toInt)
	    			
	    			keep(xs1); keep(w1); keep(ys1); keep(w2); keep(zs); keep(w3); keep(total.toString) 
	    			kept
    			}
    		}
  	}
  }
}