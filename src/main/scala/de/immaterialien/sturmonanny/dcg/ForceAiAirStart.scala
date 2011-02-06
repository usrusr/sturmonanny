package de.immaterialien.sturmonanny.dcg

import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._

/**
 * 
 * skip all takeoff waypoints to avoid problems with overcrowded or 
 * just plain MDS incompatible start positions 
 * 
 * @author ulf
 *
 */
class ForceAiAirStart(args: String) extends DoNothingMisRewriter(args) {
  override def gatherer = new ForceAiAirStart.Gatherer()
}

object ForceAiAirStart {
  /**
   * will make one pass 
   */
  protected class Gatherer() extends DoNothingMisRewriter.Gatherer with trie.TrieParsers {

    override lazy val interestingBlocks: Parser[Kept] = {
      	(iniLine("WING")
      		~ rep(wingDefinition)
          ~ rep(squadArgs ~ squadWay)
        ) ^^^ kept
    }
    lazy val wingDefinition = {
      o ~> ("[^\\[]\\S*".r ^^ { squad =>
//        println("squad:" + squad)
        squadBlock.add("["+squad+"]")
        squadWayBlock.add("["+squad+"_Way]")
        keep(squad)
      })
    }
    lazy val squadBlock = new TrieParser    
    lazy val squadWayBlock = new TrieParser    

    lazy val squadArgs = {
    		o ~> (squadBlock ^^ (x=>keep(x.mkString("")))) ~
        rep(anyLine)
    } ^^ { x =>
      println("squadArgs:" + x)
      kept
    }
    lazy val squadWay = {
    	o ~> (squadWayBlock ^^ (x=>keep(x.mkString("")))) ~
        opt("\\s*TAKEOFF.*".r) ~ // ignore first takeoff
        rep(anyLine)
    } ^^ { x =>
      println("squadWay:" + x)
      kept
    }

    def direct(str: String): Parser[String] = new Parser[String] {
      def apply(in: Input) = {
        val off = in.offset
        var i = 0
        var j = off
        while (i < str.length && j < in.source.length && str.charAt(i) == in.source.charAt(j)) {
          i += 1
          j += 1
        }
        val found = in.source.subSequence(off, j).toString
        val r = "".r
        if (i == str.length)
          Success(found, in.drop(j - off))
        else
          Failure("expected '" + str + "' but found '" + found + "'", in)
      }
    }
    //    override lazy val wayPoint: Parser[Kept]  = (
    //    	("""\s*\[.*_Chief_Road\]""".r ^^ keep) ~ extendedChiefWaypoint ~ (opt(rep(chiefWaypoint))^^^kept) 
    //    )^^^kept
    //    
    //    

    //    lazy val extendedChiefWaypoint : Parser[Kept]  = (
    //    		o ~> keepDouble ~ w ~ keepDouble ~ w ~ keepDouble ~ w 
    //    		~ (int^^{old=>
    //    			val rand = min + (max-min).toDouble * math.random
    //    			val total = math.max(old, rand.toInt)
    //    			keep(total.toString)
    //    		})  
    //    		~ w ~ keepInt ~ w ~ keepDouble 
    //    )^^^kept  	

    //    lazy val extendedChiefWaypoint : Parser[Kept]  = (
    //    		(o ~> doubleString ~ wString ~ doubleString ~ wString ~ doubleString ~ wString ~ int ^^ {
    //    			case xs1 ~ w1 ~ ys1 ~ w2 ~ zs ~ w3 ~ rs ~ w4 ~ oldTime => {  
    //	    			val rand = min + (max-min).toDouble * math.random
    //	    			val total = math.max(old, rand.toInt)
    //	    			
    //	    			keep(xs1); keep(w1); keep(ys1); keep(w2); keep(zs); keep(w3); keep(rs); keep(w4); keep(total.toString) 
    //	    			kept
    //    			}
    //    		(o~> locationToWait) ~ w ~ keepInt ~ w ~ keepDouble 
    //    )^^^kept  
    //    lazy val locationToWait : Parser[Kept] = {
    //  		doubleString ~ wString ~ doubleString ~ wString ~ doubleString ~ wString ~ int ^^ {
    //    			case xs1 ~ w1      ~ ys1          ~ w2      ~ zs           ~ w3      ~ oldTime => {
    //    				
    //    				var rand = if(grid<=0) math.random else {
    //    					def bucketize(doubleString:String)= {
    //    						(doubleString.toDouble/grid).toLong 
    //    					}
    //    					reSeedable.setSeed((bucketize(xs1) ^ seed ^ (bucketize(ys1)<<8)))
    //    					reSeedable.nextDouble 
    //    				}
    //    				
    //	    			val transformedRand = min + (max-min).toDouble * rand
    //	    			val total = math.max(oldTime, transformedRand.toInt)
    //	    			
    //	    			keep(xs1); keep(w1); keep(ys1); keep(w2); keep(zs); keep(w3); keep(total.toString) 
    //	    			kept
    //    			}
    //    		}
    //  	}
  }
}