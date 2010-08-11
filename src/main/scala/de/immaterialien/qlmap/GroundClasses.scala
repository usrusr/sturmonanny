package de.immaterialien.qlmap
import scala.util.parsing.combinator._

import org.apache.commons.lang.StringUtils


class GroundClasses(fbdjPath:String) extends Log { import java.io.File
  val (multi, static) : (Map[String, GroundClass.GC],Map[String, GroundClass.GC]) = {
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

object CsvParser extends RegexParsers with Log {
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
        parsed.map(tmp += _).getOrElse(log warn "failed to parse '"+line++"' from "+f.getAbsolutePath)
      }
      Map() ++ tmp
    }
  }
}

object GroundClass extends Enumeration with Log {
  
  case class GC(weight:Double, name:String) extends Val {
    override def toString = name
  }
  //type GroundClass = GC
//type Value
  
 val Tank = GC(10, "Tank")
 val Ship = GC(10, "Ship") 
 val Car = GC(5, "Car")
 val Fuel = GC(50, "Fuel") // a special type of car, handled in checkDescription
 val Wagon = GC(5, "Wagon") 
 val AAA = GC(6, "AAA") 
 val Artillery = GC(7, "Artillery") 
 val Bridge = GC(0, "Bridge") 
 val Ground = GC(0, "Ground") 
 val Misc = GC(0, "Misc") 
 val Plane = GC(0, "Plane") 
 val Unidentified = GC(1, "Unidentified")
  

  def parse(in: String): Value = if (in == null) Unidentified else try {
//println("iding "+in)    
    GroundClass.withName(in.trim)
  } catch {
    case nse: java.util.NoSuchElementException => {
      var best:Value = Unidentified
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
          log warn "did not find commons-lang stringutils for levenshtein distance"
          Unidentified
        }
      }
      Unidentified
    }
  }

}
