package de.immaterialien.sturmonanny.persistence

import de.immaterialien.sturmonanny._
import util.configgy

import java.net.URLEncoder


/**
 * 
 * @author ulf
 *
 */
@deprecated("most pilot names would not be valid configgy keys!")
class ConfiggyBackend extends IBalanceDao with util.Log { 
	var store : Option[ConfiggyBackend.BalanceStore] = None
	var file : Option[String] = None
	var lastSave = System.currentTimeMillis
	def load(pilot:String):Option[IBalanceDao.BalanceRB] = store map { sides => 
		var scale = sides.factor.apply.toDouble
		if(scale==0d) scale = 1
		val codedPilot = URLEncoder.encode(pilot)
		val red = sides.red(codedPilot)
		val blue = sides.blue(codedPilot)
		IBalanceDao.BalanceRB(red.toDouble/scale, blue.toDouble*scale)
	}

	def store(pilot:String, balanceRed:Option[Double], balanceBlue:Option[Double]){
		store map {sides=> 
			var updated = false
			val factor = sides.factor.apply.toDouble
			val codedPilot = URLEncoder.encode(pilot)
			def write(side:(String, Int)=>Unit, balance:Option[Double]) {
				balance map {v=>
					updated = true
					side(codedPilot, (factor*v).toInt)
				}
			}
			write(((x,y)=>sides.red.update(x,y)), balanceRed)
			write(((x,y)=>sides.blue.update(x,y)), balanceBlue)
			if(updated && System.currentTimeMillis > (lastSave + (sides.writeInterval.apply*1000))) {
				save({x:String=>log.error("autosave: " + x)})
			}
		}
	}
	
	def open(props:Map[String, String], info:String=>Unit, errorReceiver:String=>Unit){
		file = props.get("file")
		var error = errorReceiver
		if(file.isEmpty) {
			file = Some("flightminutes.store.cfg")
			
			// exceptions are ok if we use the default file 
			error = info  
		}else{
			if( ! new java.io.File(file.get).exists){
				error("file "+file+" does not exist")
				error = info
			}
		}
		
		val existing  : Option[ConfiggyBackend.BalanceStore] = try {
			file map {fname=>
				new ConfiggyBackend.BalanceStore(fname)
			}
		}catch{
			case e => { 
				error("exception while loading "+file+" "+e)
				None
			}
		}
		store = Some(existing getOrElse (new ConfiggyBackend.BalanceStore(null))) 
	}
	def close(info:String=>Unit, error:String=>Unit){
		save(error)
	}
	
	protected def save(error:String=>Unit):Unit=  synchronized{ import java.io.File
		file map {fname=>
			val f = new File(fname)
			val last = new File(fname+".bak")
			if(last.exists && ! last.delete) error("backup "+last.getAbsolutePath+" could not be deleted")
			if(f.exists && ! f.renameTo(last)) {
				error("could not move to backup "+last.getAbsolutePath+", deleting "+f.getAbsolutePath)
				if( ! f.delete) error("failed to delete "+f.getAbsolutePath)
			}
			store.flatMap(_ writeToFilesystemOrMessage f).
				// writeToFilesystemOrMessage returns None on success
				map(error(_)).getOrElse{
					lastSave = System.currentTimeMillis
					log.debug("saved to "+f.getAbsolutePath)
				}
		}
	}
}

object ConfiggyBackend {
	protected class BalanceStore(path:String) extends configgy.ConfigurationSchema(path) {
		doc = "stores full numbers of plane minutes currency * scale factor to get a little more resultion"
		object factor extends Field(1000) {doc="scale factor"}
		object writeInterval extends Field(60) {doc="the minimum time between rewrites of this file, in seconds"}
		object blue extends Table(0) { 
			
		}
		object red extends Table(0) { 
			
		}
	}
}