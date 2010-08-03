package de.immaterialien.sturmonanny.snippet;

import scala.collection.mutable
import scala.xml._
import net.liftweb.common._
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._ 

import _root_.de.immaterialien.sturmonanny.util.configgy._
import _root_.de.immaterialien.sturmonanny.global._

class LiftConf {
   def conf(html:NodeSeq)={
	   Instances.configuration.liftForm 
   }
   def instanceConf(html:NodeSeq):NodeSeq={
     val name = {
       S.param("name").openOr("")

     }
     val server = Instances.nameToInstance(name)
	   server.conf.liftForm
   }  
   def serverName(html:NodeSeq):NodeSeq={
     Text(S.param("name").openOr(""))
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
   
  def instanceLinks(html:NodeSeq):NodeSeq={
//    for(name <- Instances.nameToInstance.keys){ 
//    	bind("i", html, 
//    			"instance" -> <a href={InstanceConf.confLink(name)}>name</a>
//    	)
//    }
		<div>
			{Instances.nameToInstance.keys.map(name=>
	    	bind("i", html, 
	    			"instance" -> <a href={InstanceConf.confLink(name)}>{name}</a>
	    	)
			)}
		</div>
  }
}
object LiftConf {
  def globalConfig = GlobalConfig.singleton
//  val instances = new Instances()

   
  object isLoggedIn extends SessionVar[Boolean](false)
  def loggedIn(html:NodeSeq) = if(isLoggedIn) html else NodeSeq.Empty
  def loggedOut(html:NodeSeq) = if(isLoggedIn) NodeSeq.Empty else html

	


  
}
