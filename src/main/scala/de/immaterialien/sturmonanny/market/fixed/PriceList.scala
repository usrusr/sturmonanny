package de.immaterialien.sturmonanny.market.fixed
import net.lag.configgy
import de.immaterialien.sturmonanny.util.ConfiggyFile


class PriceList(file:String) extends ConfiggyFile(file){
	object prices extends Group{ 
//	  object host extends Field( try{java.net.InetAddress.getLocalHost.getHostName}catch{case _ => "127.0.0.1"})
	  object host extends Field( "127.0.0.1")
	  object il2port extends Field(2001)   	  
	  object consoleport extends Field(2011)
      object toolName extends Field("Sturmonanny")
	  object serverName extends Field("testserver")
	}
}
