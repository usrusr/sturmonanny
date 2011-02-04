package de.immaterialien.sturmonanny.dcg

import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._


/**
 * framework for a mission processor, the "Gatherer" parser writes through and can be overridden in parts
 * @author ulf
 *
 */
class DoNothingMisRewriter (args: String) extends javax.xml.ws.Provider[File] with Log {
	lazy val intermediateName:String={
		val ret = this.getClass.getSimpleName.replaceAll("""^(?:.*\$)?([^\$]+)(?:\$\d*)?""", "$1")
println(" intermediateName: "+ret)		
		ret
	}
	def gatherer  = new DoNothingMisRewriter.Gatherer
		  override def invoke(file: File): File = {
		
		    val g = gatherer

		    val out = new File(file.getParent, file.getName.dropRight(4) + "."+intermediateName+".tmp")
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

object DoNothingMisRewriter extends Log with  ParseUtil {
  /**
   * will make two passes: 
   * first with writer = None gathering info in the two maps
   * second with writer = Some(x) 
   * third with writer , gathering in dromesToSides 
   * @author ulf
   */
  class Gatherer extends RegexParsers {
    case object kept extends Kept
    trait Kept
    implicit def multipleKepts(ks: Seq[Kept]): Kept = kept
    implicit def seqKepts(k1: ~[Kept, Kept]): Kept = kept

    override def skipWhitespace = false

    var writer: Option[Writer] = None

    def pass[T](in: T): T = {
//println("pass '"+in+"'")     	
      writer map (_ append in.toString)
      in
    }
    val endNl   = """^(.*?)(?:[ \t]*[\r\n][ \t]*)+$""".r
    val startNl = """^(?:[ \t]*[\r\n])+(.*?)""".r
    var nlState = true
    var bofState = true
    def keep(inval: String): Kept = {

    	var in= inval match {
    		case startNl(rest) => if(nlState || bofState) rest else "\r\n" + rest
    		case _ => inval
    	}
    	if(!bofState && nlState && in.startsWith("[")) in="\r\n"+in
//println("keep '"+inval+"' -> '"+in+"' nlstate:"+nlState)       	
    	nlState = inval match {
    		case endNl(rest) => true 
    		case _ =>false
    	}

 	
    	
      writer map (_ append in)
      if( ! in.isEmpty) bofState=false
      kept
    }
    lazy val fileParser: Parser[Kept] = {
      rep(
//      		(iniLine("BornPlace") ~> bornPlaces) 
//      		| (iniLine("Chiefs") ~ rep(anyLine) ~> rep(chiefRoads)) 
      		interestingBlocks
      		| (iniLine ~ rep(anyLine)) 
      		| "\\z".r
        //) ~ "$".r  ^^^ kept
		) ~ (o ~ "\\z?".r) ^^^kept
    }

    lazy val interestingBlocks: Parser[Kept] = {
      		(iniLine("DOES_NOT_EXIST") ~> rep(anyLine)) ^^^kept
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
      o ~> ("[" ~> "[^\\]]+".r <~ "]")^^(s => keep("\r\n[" + s + "]"))
    }
    lazy val anyLine: Parser[Kept] = {
      o ~> ("[^\\[].*".r ^^ keep)
    }
    
    lazy val chiefRoads: Parser[Kept]  = (
    	("""\s*\[.*_Chief_Road\]""".r ^^ keep) ~ (rep(chiefWaypoint)^^^kept) 
    )^^^kept
    
    lazy val chiefWaypoint : Parser[Kept]  = (
    		o ~> keepDouble ~ w ~ keepDouble ~ w ~ keepDouble <~ opt( w ~ keepInt ~ w ~ keepInt ~ w ~ keepDouble) 
    )^^^kept
    lazy val bornPlaces: Parser[Kept] = {
      rep(bornPlace) ~
        rep(o ~> aerodrome) ^^^
        kept
    }
    lazy val aerodrome: Parser[Kept] = { 
      (
      		("""\s*\[BornPlace\d+\]""".r ^^ keep) ~ rep(anyLine)
      ) ^^^ kept
    }
    lazy val bornPlace: Parser[Kept] = {
      (o ~> bornPlaceSide ~ w ~ keepDouble ~ w ~ keepDouble ~ w ~ keepDouble 
      		//~ opt(rep(("[ \\t]+".r ^^ keep) ~ (int^^pass))) ~ (nl^^pass)
      		~ opt("[ \\t\\S]+".r ^^ keep) 
      )^^^ kept
    }
    lazy val bornPlaceSide: Parser[Kept] = (int ^^ pass)^^^kept
    lazy val int: Parser[Int] = {
      """-?\d+""".r ^^ (_.trim toInt)
    }
    lazy val keepInt: Parser[Kept] = {
      """-?\d+""".r ^^ keep
    }    
    lazy val keepDouble: Parser[Kept] = {
      """-?(?:(?:\d*\.\d+)|\d+)""".r ^^ keep
    }
    lazy val double: Parser[Double] = {
      """-?(?:(?:\d*\.\d+)|\d+)""".r ^^ (_.trim toDouble)
    }
    lazy val doubleString: Parser[String] = {
      """-?(?:(?:\d*\.\d+)|\d+)""".r 
    }
    lazy val wordString: Parser[String] = {
      """\S+""".r 
    }
    
    //lazy val w: Parser[Kept] = whiteSpace ^^ keep
    val wString: Parser[String] = "[ \\t]+".r 
    lazy val w: Parser[Kept] = "[ \\t]+".r ^^ keep
    
    lazy val o: Parser[Kept] = "\\s*".r ^^ (_=> keep("\r\n"))
    
    
    def matcher(reg:String):Parser[scala.util.matching.Regex.Match] = matcher(reg.r)
    def matcher(reg:scala.util.matching.Regex):Parser[scala.util.matching.Regex.Match] = new Parser[scala.util.matching.Regex.Match] {
      def apply(in: Input) = {
    		val off = in.offset
    		(reg findPrefixMatchOf (in.source.subSequence(off, in.source.length))) match {
	        case Some(matched) =>
	          Success(matched, 
              in.drop(matched.end))
	        case None =>
	          Failure("no match for "+reg+" ", in)
	      }
      }
    }
  }
}