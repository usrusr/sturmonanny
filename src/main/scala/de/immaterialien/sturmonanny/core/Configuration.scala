package de.immaterialien.sturmonanny.core

import net.lag.configgy
import de.immaterialien.sturmonanny.util.ConfiggyGroup
//import de.immaterialien.sturmonanny.core

class Configuration(val file : String) {
	
	object server  extends ConfiggyGroup{ 
	  object host extends Field( try{java.net.InetAddress.getLocalHost.getHostName}catch{case _ => "127.0.0.1"})
	  object il2port extends Field(2001)   	  
	  object consoleport extends Field(2011)
      object toolName extends Field("Sturmonanny")
	  object serverName extends Field("testserver")
	}
	object game extends ConfiggyGroup {
	  object deathpenalty extends Field(2)
	  object warningsToKick extends Field(10)
	  object warningInterval extends Field(3)
	  object startcost extends Field(10)
	  object refund extends Field(50)
	  object accountlimit extends Field(1000)
	  object recruitshare extends Field(50)
 
	}
//	object market extends ConfiggyGroup {
//	  object planesfile extends Field("planes.lst")
//	  object unlimitedquanity extends Field(50)
//	  object maxStartPrice extends Field(50)
//	  object maxPrice extends Field(100)
//	  object updatePeriod extends Field(5)
//	  object maxStep extends Field(5)
//	  object tolerance extends Field(20 )
//	} 
	object market extends ConfiggyGroup {
	  object implementation extends Field("de.immaterialien.sturmonanny.core.AllPlanesEqualMarket")
	  object configuration extends Field("planes.lst")
	} 
	
    private val all = List(server, game, market)
	def apply(conf : configgy.Config) = {
	  all foreach (_ apply conf)
	}
	try{
		this(configgy.Config.fromFile(file))
	}catch{
	  case x: _root_.java.io.IOException => x.printStackTrace
	  case x => println(""+x)
	}
	override def toString = {
	  val sb = new scala.StringBuilder
      var prefix : String= ""
	  for(group<-all){
	    val blanks = changeGroup(sb, prefix, group.prefix)
	    group.write(sb, blanks)
	    prefix = group.prefix
	  }
	  changeGroup(sb, prefix, "")
	  sb toString
	}
	private def changeGroup(sb : StringBuilder, oldP : String, newP : String):String = {
	  var oldS = List.fromString(oldP, '.')
	  var newS = List.fromString(newP, '.')
	  var count = -1
	  while (newS != Nil && oldS!=Nil &&  oldS.head == newS.head ){
	    newS = newS.tail
	    oldS = oldS.tail
	    count+=1
	  }
	  count+=oldS.length
	  for(close <- oldS.reverse){
	    sb append ("   "*count)+"</"+close+">\r\n"
	    count-=1
	  }
	  for(open<- newS){
	    count+=1
	    sb append ("   "*count)+"<"+open+">\r\n"
	  }
	  "   "*(1+count)
	} 
}
object Configuration {
   implicit def fieldReadConversionString (in : ConfiggyGroup#Field[String]) : String = in.apply
   implicit def fieldReadConversionBoolean (in : ConfiggyGroup#Field[Boolean]) : Boolean = in.apply
   implicit def fieldReadConversionInt (in : ConfiggyGroup#Field[Int]) : Int = in.apply
   
//   implicit def fieldIntString (in : Int) : String = ""+in
//   implicit def fieldIntString (in : Boolean) : String = ""+in
   
//   implicit def fieldStringConversionString (in : ConfiggyGroup#Field[String]) : String = in.apply
//   implicit def fieldStringConversionBoolean (in : ConfiggyGroup#Field[Boolean]) : String = ">>"+in.apply
//   implicit def fieldStringConversionInt (in : ConfiggyGroup#Field[Int]) : String = ">>"+in.apply
   
   object Default extends Configuration("default.conf")
}


