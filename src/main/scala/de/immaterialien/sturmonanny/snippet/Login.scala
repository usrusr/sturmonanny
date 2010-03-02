package de.immaterialien.sturmonanny.snippet

import net.liftweb.common._
import net.liftweb.http.SessionVar
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._

 
import scala.xml.{NodeSeq}
//class Login {  
object Login {
  object isLoggedIn extends SessionVar[Boolean](false)
  def valid(html:NodeSeq) = if(isLoggedIn) html else NodeSeq.Empty

	def form(html:NodeSeq)={ 
	  var user=""
	  var pass=""
	  bind("f", html, 
        "user" -> text(user, user = _),
        "pass" -> text(pass, pass = _)
        )
	}

// def conf(html:NodeSeq)={
   
//   val cfg = LiftConf.confForm(html, gc)
//   LiftConf.confForm("cfg", gc)
// }
}
