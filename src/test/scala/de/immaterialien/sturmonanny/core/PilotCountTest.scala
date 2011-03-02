package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny._

import fbdjhosting.FbdjAdapter 
import persistence.BalanceWrapper
import scala.util.parsing.combinator._

object PilotCountTest {
	class ReplayServerImpl(val initConf : String) extends Server{ Server =>
		def this() = this("default.conf") 
		private var members : List[UpdatingMember] = Nil 
		//this : Server =>
	 
		private var internalconf = new Configuration(initConf, this)   
		override def conf = internalconf  
		override def conf_= (newConf : Configuration) {  
			internalconf = newConf
			members foreach (_ updateConfiguration)
		}   
		override def shutdown = ()
		override val balance = new BalanceWrapper with Member 
		override val rules = new Rules with Member   
		override val pilots = new Pilots with Member   
		override val planes = new Planes with Member       
		//override val market = new MarketActor(conf.market.implementation.apply, conf.market.configuration.apply) with Member
//		override val market = new MarketActor("de.immaterialien.sturmonanny.market.AllPlanesCost", conf.market.configuration.apply) with Member
		override val market = new de.immaterialien.sturmonanny.market.fixed.AllPlanesCost with Member{
			def updateConfiguration=() 
		}
		override val fbdj = new FbdjAdapter with Member {   
			override def updateConfiguration = ()
		}
		override val dispatcher = new LocalizedDispatcher with Member
		override val eventlog = new EventLogDispatcher with Member
		
		override val time = new util.TimeSourceImpl
	
		override val multi = new Multiplexer ("", 0, 0) with Member {
			override def newServerThread = null
			override def newIl2Waiter = null
			override protected[this] def outWrite(line: String) = println("to server: "+line)
		}
	debug("init server class")	 
		init()
	}
	val srv=new ReplayServerImpl
	val pilots = srv.pilots
	
	def main(args: Array[String]) {
		val t1=pilots.newElement("test1")
		val t2=pilots.newElement("test2")
		val t3=pilots.newElement("test3")
		
		
		t1.currentSide=Armies.Blue
		def printFor(p:Pilots#Pilot){
			val c = pilots.roughPlayerCount(p)
			println("for "+p+": "+c)
		}
		printFor(t1)
		t2.currentSide=Armies.Blue
		printFor(t2)
		t3.currentSide=Armies.Red
		printFor(t3)
		
		
		 srv.rules.kick("testuser", "funny-reason", 10)
	}
}