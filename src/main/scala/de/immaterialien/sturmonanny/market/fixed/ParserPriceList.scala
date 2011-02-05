package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._

class ParserPriceList(list: java.io.File)  extends Log{
  //var timeTable: immutable.TreeMap[(String, String), Map[Int, Map[String, Double]]] = _
  var changeDate = 0L
	var timeTable: immutable.SortedMap[(String, String),Map[Int,Map[String,Double]]] = _//checkUpdate().getOrElse(new immutable.TreeMap)
  def checkUpdate() : Option[immutable.SortedMap[(String, String),Map[Int,Map[String,Double]]]]= {
  	if(list.lastModified>changeDate){
  		val p = new Parser
  		var r : java.io.FileReader= null
  		try{
  			r = new java.io.FileReader(list)
  			val parsed = p.parseAll(p.file, r)
println("parsed: "+parsed)  			
  			r.close
  			changeDate=list.lastModified
  			parsed match {
  				case p.Failure(msg, where) => {
  					log.error("failed to parse "+list+" "+msg+" : "+where)
  					None
  				}
  				case p.Error(msg, where) => {
  					log.error("error parsing "+list+" "+msg+" : "+where)
  					None
  				}
  				case p.Success(newTable, _) => {
  					log.info("successfully parsed "+list+" : "+newTable)
  					timeTable=newTable
  					Some(timeTable)
  				}
  			}
  			
  		}catch{
  			case x => {
  				log.error(("failed to parse "+list), x)
  				None
  			}
  		}finally{
  				if(r!=null) try{ r.close }catch{case _ => }
  		}
  	}else{
  		None
  	}
  }
  class Parser extends ParseUtil {
    override val whiteSpace = "[\\t ]+".r
    val comment = """(?:#[^\r\n]*)?""".r
    val bol = """(?:(?:#[^\r\n]*)?\s*[\r\n])+""".r //^^ (x=> println("bol:'"+x+"'"))
    lazy val file = {
    		firstBox  ~
    		rep(otherBox ) <~
    		opt(bol) ~opt(comment)
    }^^{ case first ~ rest => 
    	val all = first :: rest
    	
    	
    	val tmp = new mutable.HashMap[(String, String), Map[Int, Map[String, Double]]]
    	for( ((pair, side), list) <- all){
    		var newPairVal = tmp.get(pair).getOrElse{Map[Int, Map[String, Double]]()}
    		var newSideVal = newPairVal.get(side).getOrElse{Map[String, Double]()}
    		newSideVal = newSideVal ++ list
    		newPairVal = newPairVal + (side -> newSideVal) 
    		tmp += (pair -> newPairVal)
    	}
    	new immutable.TreeMap[(String, String), Map[Int, Map[String, Double]]] ++ tmp
    }
    
    lazy val priceLine = {
//    	bol ~> "[^#\\s]\\S*[^\\s=]".r ~ "=" ~ double ^^ {
//    		case plane ~ _ ~ price => (plane -> price)
//    	}
    	bol ~> matcher("""[ \t]*(\S+)[ \t]*=[ \t]*(-?(?:(?:\d*\.\d+)|\d+))""")^^ {
    		groups=>{
	    		val plane = groups.group(1)
	    		val price = groups.group(2).toDouble
	    		(plane -> price)
	    	}
    	}
    }
    
    
    def dateFloor(date:String):String = date+("000000000".drop(date.length)) 
    def ifNull(in:String, default:String = "")=if(in==null)default else in
    lazy val firstBox = {
    	// [ army? kampa \d* ]
    	// [ 11 99 999 ]
    	// [ 99 ]
    	// [ 99 999 ]
      matcher(
      		"""\[[ \t]*"""+
      		"""(?:(\d)[ \t]+)?""" + // army
      		"""(\S\S+)"""+ // campaign
      		"""(?:[ \t]+(\d+))?"""+ // mindate
      		"""[ \t]*\]""") ^^ { m  => 
      			val side = math.max(0, ifNull(m.group(1),"0").toInt)
      			val campaign = m.group(2)
      			val dateStr = dateFloor(ifNull(m.group(3),""))
      			((campaign, dateStr), side)
      		} 
    } ~ rep(priceLine)^^ { case box ~ lines => 
//    	val linesMap = Map[String, Double]()++lines
//    	val sidesMap = Map[Int, Map[String, Double]]()+(box._2 -> linesMap)
//    	(box._1 -> sidesMap)
    	(box, lines)
    }
    lazy val otherBox = bol ~> firstBox
  }
  override def toString = {
  	timeTable.toList.mkString("\n")
  }
}