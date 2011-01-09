package de.immaterialien.qlmap

import scala.util.parsing.combinator._
import de.immaterialien.sturmonanny.util.trie._

class MisParser(misFile: java.io.File, config: MapBase, grounds: GroundClasses) extends RegexParsers with TrieParsers with Log {
	
	override val whiteSpace = """[ \t]+""".r
	 
	lazy val eol = rep(opt(whiteSpace) ~ (
//			new Parser[Unit]{
//				override def apply(in:Input)={
////					if(in.atEnd) 
////						Success((), in)
////					else 
//						Failure("not at end", in)
//				} 
//			} | 
			"""[\n\r]+""".r
		)~opt(whiteSpace))
	
  val file = new java.io.FileReader(misFile)

  val out = new MisModel
  val parseResult = this.parseAll(fileParser, file)
  
  log debug (" reading " + misFile.getAbsolutePath + " -> " + parseResult)
  file.close

  lazy val fileParser: Parser[Unit] = { "\\s*".r  ~> 
    rep((iniLine("MAIN") ~> iniInfo) | 
        (chiefWaypoints) |
        (wingDefinition) | 
        (wingWaypoints) | 
        (iniLine("FrontMarker") ~> frontMarkers) | 
        (iniLine("BornPlace") ~> bornPlaces) | 
        (iniLine("NStationary") ~> nStationaries) | 
        (iniLine("Chiefs") ~> chiefHeaders) | 
        (iniLine("WING") ~> wingNames) | 
        (iniLine <~ rep(anyLine))
      ) ^^^ ()
  }


  def iniLine(what: String): Parser[Unit] = {
    "[" ~> direct(what) <~ direct("]") ~ eol^^ {case x=>
println("ini["+ x + "]")    	
    	()
    }
  }

  
//  lazy val newLine: Parser[Unit] = ("\r\n" | "\n\r" | "\n") ^^^ ()

  lazy val iniLine: Parser[Unit] = {
    "[" ~> "[^\\]]+".r <~ "]"~ eol ^^ (x=>
println("unknown ini["+ x + "]")     		
    )
  }
  lazy val anyLine: Parser[Unit] = {
    ("[^\\[].+".r)~ eol ^^^ ()
  }
  lazy val emptyLine: Parser[Unit] = {
    ("\\s*".r) ~ eol ^^^ ()
  }
  lazy val nStationaries: Parser[Unit] = {
    rep(nStationary) ^^^ ()
  }
  lazy val nStationary = {
    //444_Static vehicles.artillery.Artillery$_50calMG_water_US 2 12624.10 49346.27 660.00 0.0 500
    """\s*[^\[\s]+ [^\s\$]+\$""".r ~> "\\S+".r ~ int ~ double ~ double ~ double <~ double ~ ".*".r ~ eol^^ {
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
    ("""\s*""".r ~> int ~ double ~ double ~ double)<~ 
    (
    		//"""\s+[^\r\n]+""".r ~
    		opt(rep(double)) ~
    		eol
    ) ^^ {
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
    ("""\s*FrontMarker\d+""".r ~> double ~ double ~ int <~ eol) ^^ { case x ~ y ~ a =>
    	out.frontMarker(x, y, a) 
    }
  }
  lazy val chiefHeaders: Parser[Unit] = {
    rep(chiefDefinition)^^^()
  }
  lazy val chiefDefinition: Parser[Unit] = {
    //14_Chief Armor.4-PzVA 2
    ("""[^\[\s]\S*""".r ~ ("""[^\.\s]+\.""".r ~> direct("\\S+".r)) ~ int 
    <~ opt(double~double~double) 
    <~ eol) ^^ {
      case name ~ className ~ side  => {
        val cls = grounds.get(className)
        if (cls.isEmpty) {
          warn("unknown ground class " + className + " for chief "+ name)
//println("multi: "+grounds.multi)          
//println("static: "+grounds.static)          
        } else {
          out.chiefs(name).classSideClassName(cls.get, side, className)
        }
      }
//      case a~b~c => {
//println("chief abc: "+a+" / "+b+ " / "+c)   
//()
//      }
    }
  }
  lazy val chiefWaypoints : Parser[Unit] = {
    ((
    		"["~>"""([^\]\s_]+_)+Road""".r<~"]" ~ eol
    ) ~ 
     (chiefWaypoint ) ~ opt(rep(chiefWaypoint))
    )^^ {
      case nameparts ~ first ~ rest  => {
//println("chiefWaypoints abc: "+nameparts+" / "+first+ " / "+rest)      	
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
  lazy val chiefWaypoint : Parser[(Double, Double)] = {
    (double ~ double ~ double <~ opt(int ~ int ~ double)  ~ eol) ^^ {case x~y~_ => (x,y)}
  }

  lazy val wingWaypoints : Parser[Unit] = {

    ((
    		//"["~>"""([^\]\s_]+_)+Way""".r<~"]"
    		"["~> exactWingName <~direct("_Way]") ~ eol
    ) ~ 
     rep(wingWaypoint)
    )^^ {
    	
    	case wing ~ rest  => {
  		
            for(wp<-rest)  
            	wp(wing)
      }
    }
  }
  /**
   * like a literal but without ignoring leading whitespace
   * @param str
   * @return
   */
  def direct(str:String):Parser[String] = new Parser[String] {
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
        Failure("expected '"+str+"' but found '"+found+"'", in)
    }
  }
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
  def direct(reg:scala.util.matching.Regex):Parser[String] = new Parser[String] {
    def apply(in: Input) = {
    		val off = in.offset
    		(reg findPrefixMatchOf (in.source.subSequence(off, in.source.length))) match {
	        case Some(matched) =>
	          Success(in.source.subSequence(off, off + matched.end).toString, 
              in.drop(matched.end))
	        case None =>
	          Failure("no match for "+reg+" ", in)
	      }
    	
    	
//      var i = 0
//      var j = 0
//      while (i < str.length && j < in.source.length && str.charAt(i) == in.source.charAt(j)) {
//        i += 1
//        j += 1
//      }
//      val found = in.source.subSequence(0, j).toString
//      if (i == str.length)
//        Success(found, in.drop(j - in.offset))
//      else 
//        Failure("expected '"+str+"' but found '"+found+"'", in)
    }
  }

  lazy val wingWaypoint: Parser[MisModel#Wing=>Unit] = {
  	/*
  TAKEOFF 86403.95 203909.50 0 0 &0
  NORMFLY 88402.00 206909.00 1100 380.00 &0
  NORMFLY 90241.00 218455.00 1000.0 380.00 &0
  GATTACK 75733.93 244276.95 500.0 380.00 &0
  NORMFLY 88835.00 209401.00 1000.0 380.00 &0
  LANDING 86403.95 203909.50 0 0 &0
  
  NORMFLY 54197.00 231252.00 3000.0 300.00 &0
  GATTACK 61697.27 231253.52 3000.0 300.00 5_Chief 1 &0    	 
  
    NORMFLY 73763.13 64857.79 1000.00 595.00 I_KG2621 2 &0
  NORMFLY 76336.85 51468.26 1500.00 400.00 131Squadron00 5 &0
  NORMFLY 90691.91 52857.48 1500.00 400.00 131Squadron00 6 &0
  
  // problem?
  TAKEOFF 51899.93 249354.27 0 0 &0
  NORMFLY 57925.00 241165.00 4000.0 400.00 &0
  NORMFLY 65151.00 231777.00 3000.0 400.00 &0
  NORMFLY 80404.32 216200.90 3000.0 400.00 &0
  NORMFLY 81009.00 216805.00 3000.0 400.00 &0
  NORMFLY 80404.32 216200.90 3000.0 400.00 II_JG5_230 2 &0
  NORMFLY 71178.00 226088.00 3000.0 400.00 &0
  NORMFLY 63152.00 234777.00 3000.0 400.00 &0
  NORMFLY 56926.00 242665.00 3000.0 400.00 &0
  LANDING 51899.93 249354.27 0 0 &0
  
  
DCG BUG???                               ||
  NORMFLY 80404.32 216200.90 3000.0 400.00II_JG5_230 2 &0  
  
  NORMFLY 16044.57 270981.51 500.0 40.00   D &0
  	 */
    //("\\S+".r ~ double ~ double ~ double ~ doubleNoBlank ~ direct("""\s*([^&\s]+(\s+\d+)?\s+)?&""".r) ~ direct("\\d".r) <~ eol) ^^ {
  	("\\S+".r ~ double ~ double ~ double 
  			~ (matcher("""(-?(?:\d+|(?:\d*\.\d+)))\s*([^&\s]+(?:\s+\d+)?\s+)?&""") ^^^ { mtch:scala.util.matching.Regex.Match =>
  				val speed = mtch.group(1).toDouble
  				val targetStr = mtch.group(2)
  				val targetOpt = if(targetStr!=null && ! targetStr.isEmpty) Some(targetStr) else None
  				(speed, targetOpt)
  			})
  			//~ doubleNoBlank ~ direct("""\s*([^&\s]+(\s+\d+)?\s+)?&""".r)
  			
  			~ direct("\\d".r) <~ eol) ^^ {
    	case "TAKEOFF"~x~y~height~speedTargetOpt~side => {wing=>
    		wing.waypoint(MisModel.WingTakeoff(x,y))
    	}
    	case "LANDING"~x~y~height~speedTargetOpt~side => {wing=>
    		wing.waypoint(MisModel.WingLanding(x,y))
    	}    	
    	case "GATTACK"~x~y~height~speedTargetOpt~side => {wing=>
    		wing.waypoint(MisModel.WingGroundAttack(x,y))
    	}
    	case wpType~x~y~height~speedTargetOpt~side => {wing=>
    		wing.waypoint(MisModel.Waypoint(x,y))
    	}
    }
  }  
  
  lazy val wingNames : Parser[Unit] = {
  	rep("\\s*".r ~> "[^\\[\\s]\\S*[^\\]\\s]".r <~ eol)  ^^ {
  		case names => for(name<-names){
  			val gotNew = out.wings.getOrElseUpdate(name, out.wings default name)
 				exactWingName.add(name, gotNew)
  		}
  	}
  }
  lazy val wingDefinition: Parser[Unit] = {
    //14_Chief Armor.4-PzVA 2
  	(
	    ("["~>exactWingName<~direct("]" )<~ eol) ~
	    rep(wingProperty)
    ) ^^ {
      case name ~ props => {
      	//for(p<-props) p(out.wings(name))
      	for(p<-props) {
      		p(name)
      	}
      	/*
  Planes 4
  Skill0 3
  Skill1 3
  Skill2 2
  Skill3 2
  pilot0 photo.bmp
  pilot1 photo.bmp
  pilot2 photo.bmp
  pilot3 photo.bmp
  Skin3 RRG_32 GvIAP.bmp
  Class air.BF_109G6Late
  Fuel 100
  weapons default
      	 */
      }
    }
  }

  lazy val exactWingName = new TrieMapParser[MisModel#Wing]

  lazy val wingProperty: Parser[MisModel#Wing=>Unit] = {
  	// allow up to two groups after skin
  	(("""Skin\d+""".r ~ "\\S+".r ~ opt("\\S+".r)) <~ eol^^{case _ ~ _ ~ _ =>  _:MisModel#Wing=>()})  |
  	(("""[^\[\]\s]+""".r ~ """[^\]\[\s]+""".r) <~ eol ^^ {
  		case "Planes" ~ value => {wing=>
  			val count = value.toInt 
  			wing.count = count
  		}
  		case "Class" ~ value => {wing=>
  			val name = if(value.startsWith("air.")) value.substring(4) else value
  			wing.name = name
  		}
  		case a ~ b => {
//println("a:"+a+" b:"+b)  		
  		_=>()
  		}
  	})// | (("""[^\[\]\s]+""".r ~ "\\s+".r ~ """[^\[\]\s]+""".r ~ rep("\\s+".r ~ """[^\[\]\s]+""".r)) ^^^ (_=>()))
  }

  lazy val int: Parser[Int] = {
    """-?\d+[ \t]*""".r ^^ (_.trim toInt)
  }
  lazy val double: Parser[Double] = {
    //"""-?(\d+|(?:\d*\.\d+))\s+""".r ^^ (_.trim toDouble)
  	"""-?(?:(?:\d*\.\d+)|\d+)[ \t]*""".r ^^ 
  	(_.trim toDouble)
//  	{x=> println("double("+x+")"); x.trim toDouble}
  }
    lazy val doubleNoBlank: Parser[Double] = {
    //"""-?(\d+|(?:\d*\.\d+))""".r ^^ (_.trim toDouble)
    """-?(?:(?:\d*\.\d+)|\d+)""".r ^^ (_.trim toDouble)
    
  }
  lazy val iniInfo: Parser[Unit] = {
    rep(("\\s*MAP\\s".r ~> """\S+""".r <~ eol) ^^ {
      case x => {
        out.baseInfo(config.info(x))
        ()
      }
    }
      | anyLine) ^^^ ()
  }
}
