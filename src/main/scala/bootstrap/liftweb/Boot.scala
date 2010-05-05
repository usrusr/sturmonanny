package bootstrap.liftweb

import _root_.net.liftweb.util._


import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}

//import _root_.de.immaterialien.sturmonanny.core._
//import _root_.de.immaterialien.sturmonanny.model._
import _root_.javax.servlet.http.{HttpServletRequest}

import de.immaterialien.sturmonanny.snippet
import _root_.de.immaterialien.sturmonanny._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot extends util.Log{// extends net.liftweb.util.LiftLogger{
  def boot {
	  de.immaterialien.sturmonanny.StartAssembly.log.debug("initialized log")
    log.ifDebug("booting lift...")
    
    LiftRules.addToPackages("de.immaterialien.sturmonanny")

//    if (!DB.jndiJdbcConnAvailable_?)
//      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    // where to search snippet
    //LiftRules.addToPackages("de.immaterialien.sturmonanny.snippet")
//    Schemifier.schemify(true, Log.infoF _, de.immaterialien.sturmonanny.model.User)

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) ::
      Menu(Loc("Logout", List("logout"), "Logout")) :: 
      Menu(Loc("InstanceConf", List("instance/name/conf"), "InstanceConf", Hidden))::
    Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append{ _.setCharacterEncoding("UTF-8") }
    
    
    snippet.InstanceConf.registerRules 
   
    net.liftweb.util.LogBoot.loggerSetup

//      val s = new Server
log.warning("starting sturmonanny instances")    
    val instances = global.Instances.configuration
//println("sturmonanny "+global.Instances.nameToInstance.size+" instances created")    
log.ifDebug("sturmonanny "+instances.instances.toList.size+" instances created")
                       
//    S.addAround(DB.buildLoanWrapper) 
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HttpServletRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}
//
///**
//* Database connection calculation
//*/
//object DBVendor extends ConnectionManager {
//  private var pool: List[Connection] = Nil
//  private var poolSize = 0
//  private val maxPoolSize = 4
//
//  private def createOne: Box[Connection] = try {
//    val driverName: String = Props.get("db.driver") openOr
//    "org.apache.derby.jdbc.EmbeddedDriver"
//
//    val dbUrl: String = Props.get("db.url") openOr
//    "jdbc:derby:lift_example;create=true"
//
//    Class.forName(driverName)
//
//    val dm = (Props.get("db.user"), Props.get("db.password")) match {
//      case (Full(user), Full(pwd)) =>
//	DriverManager.getConnection(dbUrl, user, pwd)
//
//      case _ => DriverManager.getConnection(dbUrl)
//    }
//
//    Full(dm)
//  } catch {
//    case e: Exception => e.printStackTrace; Empty
//  }
//
//  def newConnection(name: ConnectionIdentifier): Box[Connection] =
//    synchronized {
//      pool match {
//	case Nil if poolSize < maxPoolSize =>
//	  val ret = createOne
//        poolSize = poolSize + 1
//        ret.foreach(c => pool = c :: pool)
//        ret
//
//	case Nil => wait(1000L); newConnection(name)
//	case x :: xs => try {
//          x.setAutoCommit(false)
//          Full(x)
//        } catch {
//          case e => try {
//            pool = xs
//            poolSize = poolSize - 1
//            x.close
//            newConnection(name)
//          } catch {
//            case e => newConnection(name)
//          }
//        }
//      }
//    }
//
//  def releaseConnection(conn: Connection): Unit = synchronized {
//    pool = conn :: pool
//    notify
//  }
//}


