package de.immaterialien.sturmonanny.persistence

import de.immaterialien.sturmonanny._
import util.configgy
import scala.collection._
import java.net.URLEncoder
import IBalanceDao._ 

class FileBackend  extends IBalanceDao with util.Log { import FileBackend._
	var map = new mutable.HashMap[String, BalanceRB]
	var file : Option[String] = None
	var lastSave = System.currentTimeMillis
	var writeInterval = 10
	
	def load(pilot:String):Option[IBalanceDao.BalanceRB] = {
	
			val ret = map.get(pilot)
log debug("loading for "+pilot+" " + ret + " from "+map)			
			ret
	}

	def store(pilot:String, balanceRed:Option[Double], balanceBlue:Option[Double]){
			val existing = map.get(pilot)
			
			var updated = existing map { sides =>
				(balanceRed map (_ != sides.red) getOrElse false) ||
				(balanceBlue map (_ != sides.blue) getOrElse false)
			} getOrElse true
//log debug("store existing "+existing+" vs new "+balanceRed+"/"+balanceBlue+" updated? "+updated )			
			if(updated) {
				map.put(pilot, BalanceRB(
						(balanceRed getOrElse 0d), 
						(balanceBlue getOrElse 0d) 
				))
				if(System.currentTimeMillis > (lastSave + (writeInterval*1000))){
//log debug("save existing "+existing+" vs new "+balanceRed+"/"+balanceBlue+" updated? "+updated )					
					save({x:String=>log.error("autosave: " + x)})
				}else{
//					log.debug("waiting to save "+(System.currentTimeMillis-(lastSave + (writeInterval*1000))) +" ms missing")
				}
			}
	}
	
	override def open(props:immutable.Map[String, String], info : String=>Unit, errorReceiver : String=>Unit){
		file = props.get("file")
		var error = errorReceiver
		if(file.isEmpty) {
			file = Some("flightminutes.store")
			 
			// exceptions are ok if we use the default file 
			error = info  
		}
		if( ! new java.io.File(file.get).exists){
			error("file "+file+" does not exist")
			error = info
		}else{
			for(f <- file)Parser.load(f, map)
		}
	}
	def close (info:String=>Unit, error:String=>Unit){
		save(error)
	}
	
	protected def save(error:String=>Unit):Unit=  synchronized{ 
		file map (Serializer.write(_, map, error)) getOrElse {
			error("cannot save: no file defined!")
			Unit
		}
	} 
}
//@VisibleForTest
object FileBackend extends util.Log { import scala.util.parsing.combinator._
	val utf8="UTF-8"
		val l = log
//	case class BalanceRB(red:Double, blue:Double)
	object Parser extends JavaTokenParsers{
		lazy val pilot = ("\"" + """([^"\p{Cntrl}\\]|\\["\\/bfnrt]|\\u[a-fA-F0-9]{4})*""" + "\"").r ^^ {x=>
			var escaped = x.substring(1, x.length-1)
			escaped = escaped.replace("\\\\", "\\")
			escaped = escaped.replace("\\\"", "\"")
			escaped
		}
		lazy val line:Parser[Option[(String, BalanceRB)]] = {
			(
				(pilot <~"=") ~ 
				((("red:"~>floatingPointNumber^^(_ toDouble)) ~ ("blue:"~>floatingPointNumber^^(_ toDouble)))^^{
					case r ~ b => BalanceRB(r,b)
				}) <~ (comment | empty) 
			)^^{ case p ~ b => Some((p,b)) }
		}
		lazy val comment:Parser[Option[(String, BalanceRB)]] = "\\#.*".r ^^ {in=>
//l.warning("comment '"+in+"'")	
			None
		}
		lazy val empty:Parser[Option[(String, BalanceRB)]] = "\\s*".r ^^ {in=>
//l.warning("empty '"+in+"'")			
			None
		}
		lazy val failedLine:Parser[Option[(String, BalanceRB)]] = ".*".r ^^ {in=>
//l.warning("failedLine '"+in+"'")				
			None
		}
		
		lazy val possiblyLine = comment | empty | line //| failedLine
		//lazy val allLines = repsep(possiblyLine, "\n") ^^ {
		lazy val allLines = rep(possiblyLine) ^^ {
			case x => {
//println("x = "+x);				
				x filter(_ isDefined) map (_ get)
			}
		}
		
		def load(fname:String, target:mutable.Map[String, BalanceRB]){
			target.clear()
			if( ! new java.io.File(fname).exists) {
				l.error("file not found: "+fname)
			}
//println("read: "+scala.io.Source.fromFile(fname).map((x=>""+x+"")).mkString);			
			
			val s = scala.io.Source.fromFile(fname, utf8)
			
			for(ln<-s.getLines){
//				println("parsing '"+ln+"'")
				val pr = parseAll(line, ln.trim)
				if(pr.successful) {
//println("[parsed:"+pr.get+"]")						
					val got = pr.get
					if(got.isDefined) {
						val res = got.get
//println("parse success: "+res)						
						target += res 
					}else{
						l.warning("ignoring line   '"+ln+"'")
					}
				}else{
					l.warning("failed to parse line '"+ln+"'")
				}
			}

		}
	}
	private object Serializer { import java.io._
//		val error : String=>Unit = l error _

		def write(fname:String, input:mutable.Map[String, BalanceRB], error:String=>Unit){ 
			val f = new File(fname)
println("writing to "+f)			
			val last = new File(fname+".bak")
			if(last.exists && ! last.delete) error("backup "+last.getAbsolutePath+" could not be deleted")
			if(f.exists && ! f.renameTo(last)) {
				error("could not move to backup "+last.getAbsolutePath+", deleting "+f.getAbsolutePath)
				if( ! f.delete) error("failed to delete "+f.getAbsolutePath)
			}
println("writing... "+f)			
			val fos = new FileOutputStream(fname)
			val ow = new OutputStreamWriter(fos, utf8)
			try for(item<-input){
				val rb = item._2
				var pilot = item._1
				pilot = pilot.replace("\\", "\\\\")
				pilot = pilot.replace("\"", "\\\"")
				ow append "\"" append pilot append "\" =" append 
					" red: " append (rb.red.toString) append
					" blue: " append (rb.blue.toString) append "\n"
println("written to "+f)			
			} catch{
				case e=> error("failed to write balances to "+f.getAbsolutePath+" "+e.getMessage)
			}finally {
				try ow.close
				try fos.close
			}
		}
	}
}