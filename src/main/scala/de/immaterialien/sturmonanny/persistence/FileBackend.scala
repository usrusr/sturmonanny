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
					lastSave = System.currentTimeMillis
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
		
		val files = file.get :: file.get+".bak" :: Nil
		
		// load with some (hopefully) smart priorisation between the regular file and the backup
		val variations = files map { fname =>
			if( ! new java.io.File(fname).exists){
				error("file "+file+" does not exist")
				None
			}else{
				val retMap = new mutable.HashMap[String, BalanceRB]
				val foundEnd = Parser.load(fname, retMap)
				val quality : Double = (
					(1d / fname.length.toDouble) + 
					(if(foundEnd) 1000000d else retMap.size.toDouble)
				)
				Some((quality, retMap, fname))
				
			}
		}
		if( ! variations.isEmpty){
			val existingsTuple = variations.flatten.max(Ordering.Double.on[(Double, _, _)](_ _1))
			val existings = existingsTuple._2
log.debug("read storage from "+existingsTuple._3+" : \n"+existings) 			
			map.clear()
			map ++= existings
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
		lazy val end = "end."
		lazy val possiblyLine = comment | empty | line //| failedLine
		//lazy val allLines = repsep(possiblyLine, "\n") ^^ {
		
		lazy val allLines = rep(possiblyLine) ^^ {
			case x => {
//println("x = "+x);				
				x filter(_ isDefined) map (_ get)
			}
		}
		
		def load(fname:String, target:mutable.Map[String, BalanceRB]) = {
			target.clear()
			if( ! new java.io.File(fname).exists) {
				l.error("file not found: "+fname)
			}
//println("read: "+scala.io.Source.fromFile(fname).map((x=>""+x+"")).mkString);			
			
			val s = scala.io.Source.fromFile(fname, utf8)
			var foundEnd = false
			for(ln<-s.getLines){
				foundEnd = parseAll(end, ln.trim).successful
				println("parsing '"+ln+"'")
				if( ! foundEnd){
					val pr = parseAll(line, ln.trim)
					if(pr.successful) {
	l.debug("[parsed:"+pr.get+"]")						
						val got = pr.get
						if(got.isDefined) {
							val res = got.get
	l.debug("parse success: "+res)						
							target += res 
						}else{
							l.warning("ignoring line   '"+ln+"'")
						}
					}else{
						
						l.warning("failed to parse line '"+ln+"'")
					}
				}
			}
l.debug("found end = "+foundEnd+" in "+fname)			
			foundEnd
		}
	}
	private object Serializer extends util.Log { import java.io._
//		val error : String=>Unit = l error _

		def write(fname:String, input:mutable.Map[String, BalanceRB], error:String=>Unit){ 
			val f = new File(fname)
//println("writing to "+f)			
			val last = new File(fname+".bak")
			if(last.exists && ! last.delete) error("backup "+last.getAbsolutePath+" could not be deleted")
			if(f.exists && ! f.renameTo(last)) {
				error("could not move to backup "+last.getAbsolutePath+", deleting "+f.getAbsolutePath)
				if( ! f.delete) error("failed to delete "+f.getAbsolutePath)
			}
//println("writing... "+f)			
			val fos = new FileOutputStream(fname)
			val ow = new BufferedWriter(new OutputStreamWriter(fos, utf8))
			val sw = new StringWriter append "new storage.cfg: \n"
			val writers = ow :: sw :: Nil 
			
			try{ 
				for(item<-input){
					val rb = item._2
					var pilot = item._1
					pilot = pilot.replace("\\", "\\\\")
					pilot = pilot.replace("\"", "\\\"")
					for(w<-writers) {
						w append "\"" append pilot append "\" =" append 
						" red: " append (rb.red.toString) append
						" blue: " append (rb.blue.toString) append "\n"
				  }
	//println("written to "+f)			
				}
				for(w<-writers) w append "end.\n"
				log.warning(sw.toString) 
			}catch{
				case e=> error("failed to write balances to "+f.getAbsolutePath+" "+e.getMessage)
			}finally {
				try ow.close
				try fos.close
			}
		}
	}
}