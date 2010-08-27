package de.immaterialien.sturmonanny.core

import net.liftweb.actor._
import net.liftweb.common._
import _root_.de.immaterialien.sturmonanny.util._
import _root_.de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema._
import _root_.de.immaterialien.sturmonanny.core._
 
class MarketActor(val initClassName :String, val initConfigurationPath:String) extends IMarket with LiftActor with UpdatingMember with Logging{     
	var internal : Option[IMarket] = None
	var className : String = "de.immaterialien.sturmonanny.core.AllPlanesEqualMarket"  
	var configurationPath : String = "/dev/null"
	var mission = ""
	def setServerContext(server:Server) = (this ! Msg.updateConfiguration) 
	override def updateConfiguration : Unit = this ! Msg.updateConfiguration 

	def internalUpdateConfiguration : Unit = try{ 
	  
	      val newMarket = loadMarket(conf.market.implementation, conf.market.configuration) 
	      if( (! newMarket.isEmpty) && (internal.isEmpty || (newMarket.get ne internal.get))){
	        internal = newMarket
	      }else if(configurationPath!=conf.market.configuration){
	        setConfigurationUpdatePath(conf.market.configuration) 
	      }
    }catch{
      case e => debug("while setting configuration: "+ e)
	}
	 //internal = loadMarket(initClassName, initConfigurationPath)
	 
	 setServerContext(server)
 	private def loadMarket(cls:String, cfg:String) : Option[IMarket] = { 
 	  if(cls==className && ! internal.isEmpty) internal else {
	 	  try{
debug("reconfiguring market to  "+cls)	 	    
	 		  var classLoader = Thread.currentThread.getContextClassLoader
	 		  if (classLoader == null) classLoader = getClass.getClassLoader

		 	  val c : java.lang.Class[_] = classLoader loadClass cls
		 	  if( ! (classOf[IMarket] isAssignableFrom c) ){
		 	    error(cls + " does not implement IMarket!")
debug(cls + " does not implement IMarket!")
		 	    internal
        
		 	  }else if(internal.isDefined && c.isInstance(internal.get)){
		 	    // same class
debug(cls + " turned out to be same class nontetheless!")
		 	    internal
		 	  }else{
				val nu = c.newInstance.asInstanceOf[IMarket]
debug(cls + " new instance created! "+server)
				nu setServerContext server
				if( nu.setConfiguration(cfg)){
debug(cls + " new instance configured! -> "+server.planes.items.keys.mkString)
			        nu cycle mission
			        className = cls
			        configurationPath = cfg    
				 Some(nu)
				}else{
debug(cls + " new instance not configured!")
				  internal 
				}
				 
		 	  }
		  }catch{
		    case x : Throwable => error("failed to load market class "+cls, x)
		    internal
		  } 
 	  }
	}
	def messageHandler = {
	  case Msg.getPrice(plane) => reply(Msg.getPriceResult(internal map (_ getPrice plane) getOrElse 0))
	  case Msg.addAirTime(plane, millis) => internal map (_ addAirTime(plane, millis))
	  case Msg.setConfiguration(pathToFile) => reply(Msg.setConfigurationResult(internal map (_ setConfiguration pathToFile) getOrElse false))
	  case Msg.cycle(mis) => {
	    this.mission = mis
	    internal map (_ cycle(mission))
      }
	  case Msg.updateConfiguration => internalUpdateConfiguration
	  case _ =>
	}
	override def getPrice(plane : IMarket.Loadout) : Double = {
println("get Price "+plane+ " from "+internal)
if(plane.load.isEmpty)
new Exception().printStackTrace()
val ret =
		!!(Msg.getPrice(plane), 500)
	  		.asA[Msg.getPriceResult].getOrElse(Msg.getPriceResult(0d))
	  		.price
println("got "+ret)	  		
ret	  		
	}
	override def tryPrice(plane : IMarket.Loadout) : Option[Double] = { 
	  !!(Msg.getPrice(plane), 500)
//	  		.asA[Msg.getPriceResult].getOrElse(Msg.getPriceResult(0d))
//	  		.price
	  		.asA[Msg.getPriceResult] map (_ price)
	}
	def addAirTime(plane : IMarket.Loadout, millis : Long) {
	  this ! Msg.addAirTime(plane, millis)
	}
	def setConfiguration(pathToFile : String) : Boolean ={
   	  !!(Msg.setConfiguration(pathToFile), 1000)
	  		.asA[Msg.setConfigurationResult].getOrElse(Msg.setConfigurationResult(false))
	  		.success
	}
	def cycle(mission : String) = {
	  this ! Msg.cycle(mission)
    }


 	def setConfigurationUpdatePath(pathToFile : String){
 	  if(setConfiguration(pathToFile)) 
 		  configurationPath=pathToFile
    }
} 
object Msg {
	case class getPrice(plane : IMarket.Loadout)
	case class getPriceResult(price : Double)
	case class addAirTime(plane : IMarket.Loadout, millis : Long) 
	case class setConfiguration(pathToFile : String) 
	case class setConfigurationResult(success : Boolean)
	case class cycle(mission : String)  
	case object updateConfiguration 
}
