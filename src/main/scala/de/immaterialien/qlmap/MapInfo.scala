package de.immaterialien.qlmap;

import scala.collection.mutable
import java.io
import scala.util.parsing.combinator._
/**
 * reads a mapinfo
 * 
 * constructor argument: the content of the MAP line of a .mis
 *  
 * @author ulf
 *
 */
case class MapInfo(image:io.File, errors:List[String]) 
class MapBase(baseFolder:String) {
   val folder = new io.File(baseFolder)
	class InfoParser(misFile:java.io.File) extends RegexParsers {
		lazy val parseResult:MapInfo  = {
		  val file = new java.io.FileReader(misFile)
		  val ret = this.parseAll(fileParser, file)
		  file.close
		  ret.get
      }
    	private lazy val fileParser : Parser[MapInfo]= {
  				var ret = MapInfo(null, null)
  				rep(
  						(("""image\s*=\s*""".r) ~> """\S+""".r) ^^ {x => MapInfo(new io.File(folder, x.trim), Nil)} |
  						"""^\s*\S.*$""".r  ^^ {x=>
  						  if(x.trim.isEmpty) MapInfo(null, Nil)
  						  else MapInfo(null, "unknown line: " + x :: Nil)
  						}
  				) ^^ {x =>
  				  def nonnull[T](x:T,y:T):T = if(x!=null) x else y
        
  				  x.foldLeft( MapInfo(null, Nil)) { (acc, add) => 
  				  		MapInfo(nonnull(acc.image, add.image), acc.errors ::: add.errors)
  				  }
  				}
  		}
	}
  
  
	val parsed = new mutable.HashMap[String, MapInfo]
	
   def info(mapString:String):Option[MapInfo]={
     
     var cleanedString = mapString
     if(cleanedString endsWith MapBase.suffix) cleanedString = cleanedString.substring(0, cleanedString.length-MapBase.suffixLen)
     
     cleanedString = cleanedString.replaceAll("""\W""", "_")
     
     var ret = parsed.get(cleanedString)
     
     if( ! ret.isDefined){
   	  ret = Some(new InfoParser(new io.File(folder, cleanedString+MapBase.suffix)).parseResult)
     }
     
     ret
   }
	def all={
	  
	  
	  val list = folder.list(MapBase.dotMis)
   
   
	  list map { mapinfo =>
	     val id = mapinfo.substring(0, mapinfo.length-MapBase.suffixLen)
	     info(id)
	  }
	}                            
}
object MapBase {
  val suffix=".mapinfo"
  val suffixLen = suffix.length
  val pat = (""".*\Q"""+suffix+"""\E""").r
  object dotMis extends io.FilenameFilter {
    
//    override def accept(path:io.File, fname:String):Boolean = fname match {
//      case pat(_) => true
//      case _ => false
//    }
    override def accept(path:io.File, fname:String):Boolean = pat.unapplySeq(fname).isDefined
    
  } 
}
