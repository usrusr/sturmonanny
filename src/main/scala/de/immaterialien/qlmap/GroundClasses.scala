package de.immaterialien.qlmap
import scala.util.parsing.combinator._
import de.immaterialien.sturmonanny.util
import org.apache.commons.lang.StringUtils


class GroundClasses(fbdjPath:String) extends util.Log { import java.io.File
  val (multi, static) : (Map[String, GroundClass.Value],Map[String, GroundClass.Value]) = {
    val path = new File(fbdjPath)
    if( ! path.exists){
      log error "expecting csv files in " + path.getAbsolutePath + " but it does not exist, cannot paint ground units"
      (Map(),Map())
    }else{
      def checkedFile(name:String, function:String=>Map[String, GroundClass.Value]):Map[String, GroundClass.Value]={
        val mFile = new File(path, name)
        if( ! mFile.exists) log error "expecting " + mFile.getAbsolutePath + " but it does not exist, cannot paint ground units"
        if( ! mFile.canRead) log error "cannot read" + mFile.getAbsolutePath + " -> cannot paint ground units"
        function(mFile.getAbsolutePath)
      }
      (
          checkedFile("IL2MultiObjects.csv", CsvParser.loadMultiObjects), 
          checkedFile("IL2StaticObjects.csv", CsvParser.loadStaticObjects)
      )
    }
  }
  def get(name:String) = multi.get(name) map(Some(_)) getOrElse static.get(name)
}

object CsvParser extends RegexParsers with util.Log {
  lazy val cell = {
    "\"((\"\")|[^\"])*\"".r ^^ { text =>
      text.substring(1, text.length - 1).replace("\"\"", "\"")
    } | "[^\",]+".r
  } 
  lazy val classCell = {
    cell ^^ { c => 
      GroundClass.parse(c)
    }
  } 
  
  private def checkDescription(d:String, c:GroundClass.Value) = {
    if(c == GroundClass.Car && d.toLowerCase.endsWith("_fuel")) {
      (d, GroundClass.Fuel)
    } else {
      (d, c)
    }
  }
  
  lazy val multiObjectsLine:Parser[(String, GroundClass.Value)] = { 
    cell ~","~> cell ~","~ classCell ^^ {
      case name ~_~ classCell => checkDescription(name, classCell)
    }
  }
  lazy val staticObjectsLine:Parser[(String, GroundClass.Value)] = {
    classCell ~","~ cell <~ rep(","~cell)  ^^ {
      case classCell ~ _ ~ name => {
//println(classCell +" <- ")        
        checkDescription(name, classCell)
      }
    }
  }
  def loadStaticObjects (fname:String):Map[String, GroundClass.Value] = {
    loadObjects(fname, staticObjectsLine)
  }
  def loadMultiObjects (fname:String):Map[String, GroundClass.Value] = {
    loadObjects(fname, multiObjectsLine)
  }
  private def loadObjects (fname:String, parser:Parser[(String, GroundClass.Value)]):Map[String, GroundClass.Value] = {
    val f = new java.io.File(fname)
    if( ! f.exists) Map() else {
      val s = scala.io.Source.fromFile(fname)
      val tmp = new scala.collection.mutable.HashMap[String, GroundClass.Value]
      for(line<-s.getLines) {
//println ("in: "+ line )        
        val parsed = parseAll(parser, line)
        parsed.map(tmp += _).getOrElse(log warning "failed to parse '"+line++"' from "+f.getAbsolutePath)
      }
      Map() ++ tmp
    }
  }
}

object GroundClass extends Enumeration with util.Log {
  type GroundClass = Value
  
  val Tank = Value
  val Ship = Value 
  val  Car = Value 
  val  Fuel = Value // a special type of car, handled in checkDescription
  val  Wagon = Value 
  val  AAA = Value 
  val  Artillery = Value 
  val  Bridge = Value 
  val  Ground = Value 
  val  Misc = Value 
  val  Plane = Value 
  val  Unidentified = Value
  

  def parse(in: String): Value = if (in == null) Unidentified else try {
//println("iding "+in)    
    GroundClass.withName(in.trim)
  } catch {
    case nse: java.util.NoSuchElementException => {
      var best = Unidentified
      var bestDist: Int = 5 // defines the minimum hit quality to beat Unidentified
      val lower = in.toLowerCase
      try {
        for (cur <- values) {
          val string = cur.toString.toLowerCase
          val dist = StringUtils.getLevenshteinDistance(lower, string)
          if (bestDist > dist) {
            best = cur
            bestDist = dist
          }
        }
      } catch {
        case cnfe: ClassNotFoundException => {
          log warning "did not find commons-lang stringutils for levenshtein distance"
          Unidentified
        }
      }
      Unidentified
    }
  }

}
