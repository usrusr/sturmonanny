package de.immaterialien.qlmap


import scala.util.parsing.combinator._

class MisParser(misFile: java.io.File, config: MapBase, grounds: GroundClasses) extends RegexParsers with Log {
  val file = new java.io.FileReader(misFile)

  val out = new MisModel
  val parseResult = this.parseAll(fileParser, file)
  println(" reading " + misFile.getAbsolutePath + " -> " + parseResult)
  file.close

  lazy val fileParser: Parser[Unit] = {
    rep((iniLine("MAIN") ~> iniInfo) | 
        (iniLine("FrontMarker") ~> frontMarkers) | 
        (iniLine("BornPlace") ~> bornPlaces) | 
        (iniLine("NStationary") ~> nStationaries) | 
        (iniLine <~ rep(anyLine))
      ) ^^^ ()
  }

  def iniLine(what: String): Parser[Unit] = {
    "[" ~> what <~ "]" ^^ { x => println("ini: " + x); () }
  }

  lazy val newLine: Parser[Unit] = ("\r\n" | "\n\r" | "\n") ^^^ ()

  lazy val iniLine: Parser[Unit] = {
    "[" ~> "[^\\]]+".r <~ "]" ^^ { x => println("skip ini: " + x); () }
  }
  lazy val anyLine: Parser[Unit] = {
    ("[^\\[].+".r) ^^ { x => println("ignoring " + x); () }
  }

  lazy val nStationaries: Parser[Unit] = {
    rep(nStationary) ^^^ ()
  }
//  lazy val nStationary = {
//     //"""\s*\S+ ([^\s\.\$]\.)+\$""".r ~ "\\S+.*".r ^^ {x=>
//    """\s*[^\[\s]+ [^\s\$]+\$""".r ~> "\\S+".r ~int~double~double~double <~double ~ ".*".r^^ {x=> 
//     println("-stationary->"+x)
//     }
//  }
  lazy val nStationary = {
    //444_Static vehicles.artillery.Artillery$_50calMG_water_US 2 12624.10 49346.27 660.00 0.0 500
    """\s*[^\[\s]+ [^\s\$]+\$""".r ~> "\\S+".r ~int~double~double~double <~double ~ ".*".r ^^ {
      case  className ~ side ~ x ~ y ~ height  => {
        val cls = grounds.get(className)
        if(cls.isEmpty){
          warn("unknown ground class "+className+" at "+side+" "+x+" "+y + "\n <- "+grounds.multi + "\n <- "+grounds.static)
        }else{
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
    ("""\s*""".r~> int ~ double ~ double ~ double) ^^ { 
      case a ~ h ~ x ~ y => {
        println("bp:" + a + "@" + x + "/" + y); 
        out.bornPlace(a, x, y)
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
