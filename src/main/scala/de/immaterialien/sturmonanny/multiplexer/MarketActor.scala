package de.immaterialien.sturmonanny.multiplexer

import net.liftweb.actor._
import net.liftweb.common._
import de.immaterialien.sturmonanny.util._

class MarketActor extends IMarket with LiftActor with UpdatingMember with Logging{
	var internal : Option[IMarket] = None
	var className : String = "de.immaterialien.sturmonanny.multiplexer.AllPlanesEqualMarket"
	var configurationPath : String = "/dev/null"
	def updateConfiguration : Unit = {

      val newMarket = loadMarket(conf.market.implementation)
      if( (! newMarket.isEmpty) && (newMarket.get ne internal.get)){
        internal = newMarket
        className = conf.market.implementation
        
        setConfigurationUpdatePath(conf.market.configuration)
      }else if(configurationPath!=conf.market.configuration){
        setConfigurationUpdatePath(conf.market.configuration)
      }
	}
 	private def loadMarket(cls:String) : Option[IMarket] = {
 	  if(cls!=className || internal.isEmpty) internal else {
	 	  try{
		 	  val c : java.lang.Class[_] = java.lang.Class.forName(cls)
		 	  if( ! (classOf[IMarket] isAssignableFrom c) ){
		 	    error(cls + " does not implement IMarket!")
		 	    internal
		 	  }else if(c.isInstance(internal.get)){
		 	    // same class
		 	    internal
		 	  }else{
		 	    Some(classOf[IMarket].cast(c.newInstance))
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
	  case _ =>
	}
	def getPrice(plane : String) : Double = { 
	  !!(Msg.getPrice(plane), 500)
	  		.asA[Msg.getPriceResult].getOrElse(Msg.getPriceResult(0d))
	  		.price
	}
	def addAirTime(plane : String, millis : Long) {
	  this ! Msg.addAirTime(plane, millis)
	}
 	def setConfigurationUpdatePath(pathToFile : String){
 	  if(setConfiguration(pathToFile)) 
 		  configurationPath=pathToFile
    }
	def setConfiguration(pathToFile : String) : Boolean ={
//	  this ! Msg.setConfiguration(pathToFile)
   	  !!(Msg.setConfiguration(pathToFile), 1000)
	  		.asA[Msg.setConfigurationResult].getOrElse(Msg.setConfigurationResult(false))
	  		.success
       
	} 
} 
object Msg {
	protected case class changeMarket(cls:String)
	protected case class configureMarket(cls:String)
	case class getPrice(plane : String)
	case class getPriceResult(price : Double)
	case class addAirTime(plane : String, millis : Long) 
	case class setConfiguration(pathToFile : String) 
	case class setConfigurationResult(success : Boolean) 
}
