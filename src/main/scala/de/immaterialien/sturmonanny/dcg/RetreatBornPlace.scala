package de.immaterialien.sturmonanny.dcg

import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._

//class AllPlanesEverywhere(args: String) extends javax.xml.ws.Provider[File] with Log {
class RetreatBornPlace(args: String) extends DoNothingMisRewriter(args) { import RetreatBornPlace._
	 
  override def invoke(file: File): File = {

    val g = new RetreatBornPlace.Gatherer(args)
    val f1 = new FileReader(file)
    val pr1 = g.parseAll(g.fileParser, f1)
    f1.close

    val minDistance = distancePattern.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(1000)
    val minRemaining = minRemainingPattern.findFirstMatchIn(args).map(_.group(1).toInt).getOrElse(2)
//    {
//      
//    }
    
    for((side,places)<-g.sidesToPlaceLocs){
    	val placesWithDistance = places.map(place=>{
	    	val closestDistance = g.sidesToMarkers.filter(_._1 != side).flatMap(_._2).map(marker=> {
	    		val dx=(marker._1 - place._1)
	    		val dy=(marker._2 - place._2)
	    		val dist=math.sqrt((dx*dx)+(dy*dy))
	    		dist
	    	}).firstOption.getOrElse(java.lang.Double.MAX_VALUE)
    		(place, closestDistance)
    	})
    	val fromRadius = placesWithDistance.filter(_._2 < minDistance).toList
    	val toDestroy : List[(Double,Double,Double)]= if(placesWithDistance.size-fromRadius.size < minRemaining){
    		placesWithDistance.sorted(Ordering[Double].on[(_,Double)](_._2)).dropRight(minRemaining).map(_ _1)
    	}else{
    		fromRadius
    	}.map(_._1).toList
    	
    	val rest = g.destroy.get(side).getOrElse(Nil)
    	val allDestroy = toDestroy ::: rest
    	g.destroy += (side -> allDestroy )
    	
    }
    

    log debug "destroying " + g.destroy
    val out = new File(file.getParent, file.getName.dropRight(4) + "." + intermediateName + ".mis")
    if (out.exists) out.delete
    val w = new FileWriter(out)
    g.writer = Some(w)
    val f2 = new FileReader(file)
    val pr2 = g.parseAll(g.fileParser, f2)
    f2.close
    w.close
    log debug "" + pr2
    out

    val bak = new File(file.getParent, file.getName + ".pre." + intermediateName)
    if (bak.exists) bak.delete
    file.renameTo(bak)
    out.renameTo(file)
    file
  }
}

object RetreatBornPlace {
	val distancePattern = """(?i)(?:enemy)?distance\s*=\s*(\d+)""".r
	val radiusPattern = """(?i)(?:destruction)?radius\s*=\s*(\d+)""".r
	val minRemainingPattern = """(?i)min(?:Remaining)?\s*=\s*(\d+)""".r
	val replacementRedPattern = """(?i)reddummy\s*=\s*(\S+)""".r
	val replacementBluePattern = """(?i)bluedummy\s*=\s*(\S+)""".r
  /**
   * will make two passes: 
   * first with writer = None gathering info in the two maps
   * second with writer = Some(x) 
   * third with writer , gathering in dromesToSides 
   * @author ulf
   */
  protected class Gatherer(args:String) extends DoNothingMisRewriter.Gatherer {
		// the factor 2 is needed because we are interested in distance to front, not distance to frontMarker
		val radius = 2 * radiusPattern.findFirstMatchIn(args).map(_.group(1) toInt).getOrElse(500) 
		def replacement(patt:scala.util.matching.Regex)={
			val matches = patt.findAllIn(args)
			val ret = matches.matchData.map(_ group 1).toList
			if(ret.isEmpty) List("vehicles.artillery.Artillery$MG42") else ret
		}
		val redReplacements = replacement(replacementRedPattern)
		val blueReplacements = replacement(replacementBluePattern)
		
		val allReplacements=Map(1->redReplacements, 2->blueReplacements)

    var sidesToPlaceLocs = Map[Int, List[(Double, Double, Double)]]()
//    var counterToBornPlaceLocs = new scala.collection.mutable.ArrayBuffer[(Int,(Double, Double, Double))]()
    var sidesToMarkers = Map[Int, List[(Double, Double)]]()
    
    /**
     * include height, to make identification for emptying easy
     */
    var destroy = Map[Int, List[(Double, Double, Double)]]()

    override lazy val interestingBlocks: Parser[Kept] = (
    		(iniLine("BornPlace") ~> bornPlaces) ^^^ kept
      | (iniLine("FrontMarker") ~> rep(frontmarker)) ^^^ kept
      | (iniLine("NStationary") ~> rep(nstationary)) ^^^ kept
    )

    /**
     * true if x,y are inside the radius of a destroyed position
     * 
     * @param x
     * @param y
     * @param side
     * @return
     */
    def isDestroyed(x:Double,y:Double, side:Int) : Boolean = {
    	val destroyeds = destroy.get(side).getOrElse(Nil)
  		val safe = destroyeds.projection.map(e=> {
  			val dx = x - e._1
  			val dy = y - e._2
  			val dist = math.sqrt(dx*dx + dy*dy)
//println("dsit "+dist+" < "+radius)  			
  			dist
  		}).filter(_ < radius).isEmpty
  		! safe
		}
    // 167_Static vehicles.artillery.Artillery$Zenit25mm_1940 1 46298.88 217773.49 560.00 0.0 0
		lazy val nstationary4: Parser[Kept] = (
				//o ~ (wordString ^^ keep) ~ w ~ wordString ~ wString ~ int ~ doubleString ~ wString ~ doubleString ~ rep(w ~ (wordString ^^ keep) )
				o ~ (wordString^^keep)  ~ w ~> wordString ~ wString ~ int ~ wString ~ rep(w ~ (wordString ^^ keep) )
		)^^^kept
    lazy val nstationary: Parser[Kept] = (
      o ~ (wordString ^^ keep) ~ w ~> 
      (wordString ~ wString ~ int ~ wString ~ doubleString ~ wString ~ doubleString ^^ {
        case typeString ~ w1 ~ side ~ w2 ~ xs ~ w3 ~ ys => {

        	val newType = if(
        			writer.isEmpty 
        			|| ( ! typeString.startsWith("vehicles.artillery."))
        			|| ( ! isDestroyed(xs.toDouble, ys.toDouble,side))
        	){
        		typeString
        	}else{
        		allReplacements.get(side) match {
        			case None => typeString
        			case Some(Nil) => typeString
        			case Some(repl :: Nil) => repl
        			case Some(lst:List[String]) => {
        				val rnd = scala.util.Random.nextInt(lst.length)
        				val ret = lst(rnd)
        				ret
        			}
        		}
        	}
        	keep(newType);keep(w1);keep(""+side);keep(w2);keep(xs);keep(w3);keep(ys)
        }
      }) <~ rep(w ~ (wordString ^^ keep) )
    )^^^kept

    
    //  	[FrontMarker]
    //  FrontMarker0 137901.06 124688.94 2
    //  FrontMarker1 136261.09 128354.97 2
    //  FrontMarker2 136701.39 129548.56 2
    //  FrontMarker3 134703.33 132196.00 2
    lazy val frontmarker: Parser[Kept] = {
      o ~ (wordString ^^ keep) ~ w ~> (doubleString ~ wString ~ doubleString ~ wString ~ int) ^^ {
        case xs ~ w1 ~ ys ~ w2 ~ side => {
        	if (writer.isEmpty){
	          val newList = (xs.toDouble, ys.toDouble) :: sidesToMarkers.get(side).getOrElse(Nil)
	          sidesToMarkers = sidesToMarkers + (side -> newList)
        	}
          keep(xs); keep(w1); keep(ys); keep(w2); keep(side.toString)
        }
      }
    }
//    override lazy val aerodrome: Parser[Kept] = (
//      (o ~> ("[BornPlace" ~> int <~ "]") ~
//        rep("""\s*[^\[\s]\S*""".r)) ^^ {
//        case number ~ planes => {
//          writer map { w =>
//            w append ("\r\n[BornPlace" + number + "]\r\n")
//            //            
//            //            val side = dromesToSides(number)
//            //            val sp = sideToPlanes.get(side)
//            val (sid,xyz) = counterToBornPlaceLocs(number)
////            val distance :Double = closestEnemyDistance(xyz._1,xyz._2,sid)
//            val destroyed = destroy(sid).contains(xyz)
//            if( ! destroyed){
//            	for (p <- planes) keep (p)
//            }else{
//            	// AI only
//            }
//            kept
//          } getOrElse {
//            kept
//          }
//        }
//      }
//    )

    override lazy val bornPlace: Parser[Kept] =
      (o ~> int ~ wString ~ doubleString ~ wString ~ doubleString ~ wString ~ doubleString ~
      		//  MDS addon: 1 1000 200 11 0 500 12000 30 0 0 0 ___1___ 0 3.8 0 0 0
      		 opt(repN(11, wString ~ doubleString) ~ wString ~ int ~ "[ \\t\\S]+".r) 
      		^^ {
        case side ~ w1 ~ hs ~ w2 ~ xs ~ w3 ~ ys ~ mdsOpt => {
        	var disableStart = 0
        	val x=xs.toDouble
        	val y=ys.toDouble
        	val z=hs.toDouble
        	val xyz=(x,y,z)

        	if (writer.isEmpty){
	          val newList = xyz :: sidesToPlaceLocs.get(side).getOrElse(Nil)
	          sidesToPlaceLocs = sidesToPlaceLocs + (side -> newList)
	          //	          counterToBornPlaceLocs += ((side, xyz))
        	}
        	
          keep(side.toString); keep(w1); keep(ys); keep(w2); keep(xs); keep(w3); keep(ys)
          mdsOpt match {
          	case Some(rep11 ~ w4 ~ noSpawnIn ~ rest) => {
          		val noSpawnOut = if(writer.isEmpty) noSpawnIn else {
          			if(destroy(side).contains(xyz)) 1 else 0
          		}
          		for((iw ~ id) <- rep11){
          			keep(iw);keep(id)
          		}
          		keep(w4)
          		keep(noSpawnOut.toString)
          		keep(rest)
          	}
          	case _ => kept 
          }

          
        }
      }
    )
  }
}