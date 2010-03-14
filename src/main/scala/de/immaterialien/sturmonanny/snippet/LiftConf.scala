package de.immaterialien.sturmonanny.snippet;

import scala.collection.mutable
import scala.xml._
import net.liftweb.common._
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._ 

import de.immaterialien.sturmonanny.util.configgy._
import de.immaterialien.sturmonanny.global._
class LiftConf {
   def conf(html:NodeSeq)={
 
	  LiftConf.instances.liftForm 
   }  
   def loggedIn(html:NodeSeq) = if(LiftConf.isLoggedIn) html else NodeSeq.Empty
   def loggedOut(html:NodeSeq) = if(LiftConf.isLoggedIn) NodeSeq.Empty else html
   def logout(html:NodeSeq)={
     LiftConf.isLoggedIn.set(false)
     S.notice("logged out")
     S.redirectTo("/index")
     NodeSeq.Empty
   } 
   def login(html:NodeSeq)={ 
	  var user=""
	  var pass=""
	  bind("f", html, 
        "user" -> SHtml.text(user, user = _),
        "pass" -> SHtml.password(pass, pass = _),
        "submit" -> SHtml.submit("Login", () => {
          LiftConf.isLoggedIn.set(LiftConf.globalConfig.admin.hash.apply == GlobalConfig.hashPass(user, pass))
          if(LiftConf.isLoggedIn.get) {
            //RedirectResponse("/index")
            
          }else{
            S.error("wrong user or password")
          }
          
          println("config is "+LiftConf.globalConfig)
        })
    )
	}	   
}
object LiftConf {
  val globalConfig = new GlobalConfig()
  val instances = new Instances()

   
  object isLoggedIn extends SessionVar[Boolean](false)
  def loggedIn(html:NodeSeq) = if(isLoggedIn) html else NodeSeq.Empty
  def loggedOut(html:NodeSeq) = if(isLoggedIn) NodeSeq.Empty else html

	


  
}
