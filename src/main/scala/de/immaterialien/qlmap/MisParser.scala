package de.immaterialien.qlmap

import scala.util.parsing.combinator._

class MisParser(misFile: java.io.File, config: MapBase, grounds: GroundClasses) extends RegexParsers with Log {
  val file = new java.io.FileReader(misFile)

  val out = new MisModel
  val parseResult = this.parseAll(fileParser, file)
  log debug (" reading " + misFile.getAbsolutePath + " -> " + parseResult)
  file.close

  lazy val fileParser: Parser[Unit] = {
    rep((iniLine("MAIN") ~> iniInfo) | 
        (chiefWaypoints) | 
        (iniLine("FrontMarker") ~> frontMarkers) | 
        (iniLine("BornPlace") ~> bornPlaces) | 
        (iniLine("NStationary") ~> nStationaries) | 
        (iniLine("Chiefs") ~> chiefHeaders) | 
        (iniLine <~ rep(anyLine))
      ) ^^^ ()
  }

  def iniLine(what: Parser[String]): Parser[Unit] = {
    "[" ~> what <~ "]" ^^^ ()
  }

  lazy val newLine: Parser[Unit] = ("\r\n" | "\n\r" | "\n") ^^^ ()

  lazy val iniLine: Parser[Unit] = {
    "[" ~> "[^\\]]+".r <~ "]" ^^^ ()
  }
  lazy val anyLine: Parser[Unit] = {
    ("[^\\[].+".r) ^^^ ()
  }
  lazy val emptyLine: Parser[Unit] = {
    ("\\s*".r) ^^^ ()
  }
  lazy val nStationaries: Parser[Unit] = {
    rep(nStationary) ^^^ ()
  }
  lazy val nStationary = {
    //444_Static vehicles.artillery.Artillery$_50calMG_water_US 2 12624.10 49346.27 660.00 0.0 500
    """\s*[^\[\s]+ [^\s\$]+\$""".r ~> "\\S+".r ~ int ~ double ~ double ~ double <~ double ~ ".*".r ^^ {
      case className ~ side ~ x ~ y ~ height => {
        val cls = grounds.get(className)
        if (cls.isEmpty) {
          warn("unknown ground class " + className + " at " + side + " " + x + " " + y + "\n <- " + grounds.multi + "\n <- " + grounds.static)
        } else {
          //println("known ground: "+cls.get+" at "+side)
          out.groundUnit(cls.get, side, x, y)
        }
      }
    }
  }
  lazy val bornPlaces: Parser[Unit] = {
    rep(bornPlace) ^^^ ()
  }
  lazy val bornPlace: Parser[Unit] = {
    ("""\s*""".r ~> int ~ double ~ double ~ double) ^^ {
      case a ~ h ~ x ~ y => {
        out.bornPlace(a, x, y)
        out.airfield(a, x, y)
        // notify ground units of airfield presence
        out.groundUnit(GroundClass.Airfield, a, x, y)
      }
    }
  }
  lazy val frontMarkers: Parser[Unit] = {
    rep(frontMarker) ^^^ ()
  }
  lazy val frontMarker: Parser[Unit] = {
    ("""\s*FrontMarker\d+""".r ~> double ~ double ~ int) ^^ { case x ~ y ~ a => out.frontMarker(x, y, a) }
  }
  lazy val chiefHeaders: Parser[Unit] = {
    rep(chiefDefinition
//   )  ~> rep((
////opt(rep(emptyLine))~>
//        chiefNameIni ~ 
//            (
////opt(rep(emptyLine))~>
//            waypoint <~ double ~ double ~ double) ~
//            rep(
////opt(rep(emptyLine))~>
//            waypoint
//            ) 
//            
//        ) ^^ {
//          case name ~ first ~ rest  => {
//println("name:"+name+" first:"+first+" rest:"+rest)            
//            val chief = out.chiefs(name)
//            chief.waypoint(first)
//            for(wp<-rest) chief.waypoint(wp)
//          }
//        }
//    )
    )^^^()
  }
  
  lazy val chiefWaypoints : Parser[Unit] = {
    ((
    		"["~>"""([^\]\s_]+_)+Road""".r<~"]"
    ) ~ 
     (waypoint <~ int ~ int ~ double ) ~ opt(rep(waypoint))
    )^^ {
      case nameparts ~ first ~ rest  => {
        val name = nameparts.substring(0, nameparts.length-5)
            val chief = out.chiefs(name)
            chief.waypoint(first)
            for(wp<-rest.get) chief.waypoint(wp)
      }
    }
//    		^^{x=>
//println("id "+x)      
//      ()
//    }
  }
  lazy val waypoint : Parser[(Double, Double)] = {
    (double ~ double ~ double) ^^ {case x~y~_ => (x,y)}
  }


  
  lazy val chiefDefinition: Parser[Unit] = {
    //14_Chief Armor.4-PzVA 2
    "\\s*".r~>"[^\\[\\s]\\S*".r ~ ("""[^\.]+\.""".r ~> "\\S+".r) ~ int ^^ {
      case name ~ className ~ side => {
        val cls = grounds.get(className)
        if (cls.isEmpty) {
          warn("unknown ground class " + className + " for chief "+ name)
println("multi: "+grounds.multi)          
println("static: "+grounds.static)          
        } else {
          out.chiefs(name).classSide(cls.get, side)
        }
      }
    }

  }

  lazy val int: Parser[Int] = {
    """-?\d+\s+""".r ^^ (_.trim toInt)
  }
  lazy val double: Parser[Double] = {
    """-?(\d+|(:?\d*\.\d+)?)\s+""".r ^^ (_.trim toDouble)
  }
  lazy val iniInfo: Parser[Unit] = {
    rep(("\\s*MAP\\s".r ~> """\S+""".r) ^^ {
      case x => {
        out.baseInfo(config.info(x))
        ()
      }
    }
      | anyLine) ^^^ ()
  }
}
