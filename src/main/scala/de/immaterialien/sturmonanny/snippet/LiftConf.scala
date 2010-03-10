package de.immaterialien.sturmonanny.snippet;

import scala.collection.mutable
import scala.xml._
import net.liftweb.common._
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import de.immaterialien.sturmonanny.util.configgy._

class LiftConf {
   def conf(html:NodeSeq)={
 
	  LiftConf.gc.liftForm 
   }  
}
object LiftConf {
  //example
  val gc = new de.immaterialien.sturmonanny.global.GlobalConfig()


	
}
