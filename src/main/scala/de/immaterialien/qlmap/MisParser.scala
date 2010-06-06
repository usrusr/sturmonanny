package de.immaterialien.qlmap

import de.immaterialien.sturmonanny.util
import scala.util.parsing.combinator._

class MisParser(misFile:java.io.File, config:MapConfig) extends RegexParsers with util.Logging {
	val file = new java.io.FileReader(misFile)
	
	val out = new MisModel
	val parseResult  = this.parseAll(fileParser, file)
	
	file.close
 
 
	lazy val fileParser : Parser[Unit]= {
	  rep(
			(iniLine("FrontMarker") ~> frontMarkers) |
	   	(iniLine("BornPlace") ~> bornPlaces) |
	   	iniLine <~ rep(anyLine)
	  ) ^^^ ()
   }

 	def iniLine(what:String) : Parser[Unit]= {
	  "[" ~> what <~ "]" ^^ {x => println("ini: "+x); ()}
	}

	lazy val newLine : Parser[Unit]= ("\r\n" | "\n\r" | "\n") ^^^ ()
 
	lazy val iniLine : Parser[Unit]= {
	  "[" ~> "[^\\]]+".r <~ "]"  ^^ {x => println("skip ini: "+x); ()}
	}
	lazy val anyLine : Parser[Unit]= {
	  ("[^\\[].+".r ) ^^ {x => println("skipping "+x); ()}
	}
	lazy val bornPlaces : Parser[Unit]= {
	  rep(bornPlace) ^^^ ()
	}
	lazy val bornPlace : Parser[Unit]= {
	  (int ~ double ~ double ) ^^ {case a ~ x ~ y => out.bornPlace(a, x, y)}
	} 
	lazy val frontMarkers : Parser[Unit]= {
	  rep(frontMarker) ^^^ ()
	} 
	lazy val frontMarker : Parser[Unit]= {
	  ("""FrontMarker\d+""".r ~> double ~ double ~ int ) ^^ {case x ~ y ~ a => out.frontMarker(x, y, a)}
	} 
 
	lazy val int : Parser[Int]={
	  """-?\d+\s+""".r ^^ (_.trim toInt)
	} 
	lazy val double : Parser[Double]={
	  """-?(\d+|(:?\d*\.\d+)?)\s+""".r ^^ (_.trim toDouble)
	}
}
