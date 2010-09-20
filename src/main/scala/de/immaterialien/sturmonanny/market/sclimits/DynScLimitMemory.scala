package de.immaterialien.sturmonanny.market.sclimits

import scala.util.parsing.combinator._
import java.io._

object DynScLimitMemory extends JavaTokenParsers {

	def store(sides : Map[Int,Map[String, Double]], fname:File) {
		var out = new FileWriter(fname)
		for((side, planes)<-sides){
			out.append("army "+side+"\r\n")
			for((name, count)<-planes) out.append(" \""+name+"\" : "+count+"\r\n")
		}
		out.close
	}
	def load(fname:File) : Map[Int,Map[String, Double]] = {
		val in = new FileReader(fname)
		val ret = parser.parseAll(parser.wholeFile, in)
		in.close
		ret.get
	}
	private object parser extends JavaTokenParsers { 
		lazy val armyLine : Parser[Int] = "army" ~> wholeNumber ^^ (_ toInt)
		lazy val planeLine : Parser[(String, Double)] = (
			stringLiteral ~ ":" ~ floatingPointNumber  
		) ^^{ case name~ _ ~ cnt => (name.substring(1, name.length-1), cnt.toDouble) }
	
	  lazy val wholeArmy : Parser [(Int, Map[String, Double])] = (
	  	armyLine ~ rep(planeLine)
	  ) ^^ { case side ~ pairs =>
	  	(side, Map()++pairs)
	  }
	  lazy val wholeFile : Parser [Map[Int,Map[String, Double]]] = (
	  	rep(wholeArmy)
	  ) ^^ { armies =>
	  	Map()++armies
	  }
	}
}