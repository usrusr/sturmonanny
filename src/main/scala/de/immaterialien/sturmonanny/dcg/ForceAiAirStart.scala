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
  protected class Gatherer() extends DoNothingMisRewriter.Gatherer with trie.TrieParsers with Log {

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
    /*
    TAKEOFF 11118.04 142103.59 0 0 &1
		NORMFLY 11233.00 143706.00 4500.0 300.00 &1
    */
    val takeoffs=new mutable.HashMap[(Int, Int), Int]
    
    lazy val takeoffpair = {
    	o~>"TAKEOFF" ~wString~ double ~wString~ double ~wString~ double ~wString~ double ~"""([ \t]+[^&\s]\S*)*[ \t]+&\d+""".r ~
    	 o~"NORMFLY" ~wString~ double ~wString~ double ~wString~ double ~wString~ double ~"""([ \t]+[^&\s]\S*)*[ \t]+&\d+""".r  
    } ^^ {
    	case _ ~_~ tx ~_~ ty ~_~ th ~_~ _  ~ _ ~
    	  _ ~_ ~_~ nx ~_~ ny ~_~ nh ~_~ sn ~radio => {
    	  	val divisor=10000
    	  	val bin =((nx/divisor).toInt,(ny/divisor).toInt)
    	  	val binCount = takeoffs.get(bin).getOrElse(0)
//println("takeoff to "+bin)
					takeoffs += (bin -> (binCount + 1))
					
					val interpolate = 1D/(10D+binCount)
					
					val ix = tx + (nx-tx)*interpolate
					val iy = ty + (ny-ty)*interpolate
					def dotTwo(double:Double) = {
    	  		val string = "%1.00f" format double
    	  		if(string.contains(".")) string else string+".00"
    	  	}
					def dotOne(double:Double) = {
    	  		val string = "%1.0f" format double
    	  		if(string.contains(".")) string else string+".0"
    	  	}
					//val ih = nh + 10*binCount
					val minTh = math.max(500, th)
					val dynamicbonus = ((nh-minTh)*0.01)*binCount
					val ih = minTh + (nh-minTh)*interpolate + 50*binCount + dynamicbonus
					
					val ret1 = "NORMFLY "+dotTwo(ix)+" "+dotTwo(iy)+" "+dotOne(ih)+" "+dotTwo(sn)+ radio 
					val ret2 = "NORMFLY "+dotTwo(nx)+" "+dotTwo(ny)+" "+dotOne(nh)+" "+dotTwo(sn)+ radio
					
//println("interpolate: "+interpolate)					
log.debug("was takeoff: "+ret1)					
log.debug("was normfly: "+ret2)
					keep(ret1)
					keep("\r\n")
					keep(ret2)
					keep("\r\n")
    	  }
    }
    lazy val squadWay = {
    	o ~> (squadWayBlock ^^ (x=>keep(x.mkString("")))) ~
        //opt("\\s*TAKEOFF.*".r) ~ // ignore first takeoff
    		opt(takeoffpair) ~ 
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
  }
}