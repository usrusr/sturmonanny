package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util.Logging
import scala.io._
import scala.collection.mutable


object I18nReader{
  private val propertiesLine = """^\s*(\S*?)[123456789]?\s+(\S.*)$""".r
}
trait I18nReader extends Logging {
	var serverPath : String= null
	def i18nSuffix =  """/i18n/netmessages.properties"""  
  
	def init(newPath:String) = {
	  val i18nPath = newPath+i18nSuffix
	  try{
	  	val file = new java.io.File(i18nPath)
debug("init I18nReader from '"+i18nPath+"' ("+file.getAbsolutePath+" "+(if(file.exists) "exists" else "is missing")+") ")				      

	  	val source = Source.fromFile(file)// scala.io.BufferedSource.fromFile(i18nPath)
		  val lines = source.getLines.toList
//debug("in '"+i18nPath+"' ("+file.getAbsolutePath+") #lines: "+lines.length)				      
		  
		  var translations = new mutable.HashMap[String, List[String]]()
		  for(line : String<- lines) {
//debug("i18n line '"+line+"'")		
		    line.stripLineEnd match{
			    case I18nReader.propertiesLine(name, text) => {
//debug("parsed constant '"+name+"' <- '"+text+"'")				      
			      translations.get(name) match {
			        	case None => translations.put(name, List(text))
			        	case Some(existing) => translations.put(name, text::existing)
			      } 
			    }
			    case x => if( ! x.isEmpty) debug("failed to parse '"+x+"'")	 
			  }
		  }
//debug("parsed i18n message definitions: "+translations)

		  updateTranslations(translations)
    
	      serverPath = newPath

      }catch{
        case e:Exception=>if(serverPath!=null) error("Failed to parse i18n message definitions from "+i18nPath+":", e)
      }
	} 
	init(serverPath)
	def updateTranslations(translations : mutable.Map[String, List[String]])
 
}
