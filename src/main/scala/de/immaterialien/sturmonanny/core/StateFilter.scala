package de.immaterialien.sturmonanny.core

import de.immaterialien.sturmonanny.util._
import net.liftweb._


class StateFilter(multi:TimedLiftActor) extends Log {
  var pending : Set[String] = Set()
  var blocked = false
	def pass(msg:Multiplexer#UpMessage):Boolean = {
	  var pass = ! blocked
    val lowerLine = msg.line.trim.toLowerCase
	  
		if(lowerLine.startsWith("mission ")){
	    pass = true
      blocked = true
      if(lowerLine.endsWith(" begin")){
        blocked = false
        pending = Set()
      }
	  }else if(lowerLine.startsWith("difficulty ")) {
	    pass = true
	  }

    if( ! pass){

log.debug("skipping msg "+msg + " pending:"+pending.mkString(" - "))      
      
      if( ! pending.contains(lowerLine)){
        pending = pending + lowerLine
        
        // re-enqueue
        actor.LAPinger.schedule(multi, msg, 1000L)
        multi ! msg 
      }
    } 
	  pass  
	}
 
 
 
 
 
 
 
 
 
	def loadLine(in:String):String = {
	  var ret = in.trim
	  val missionLOAD = "mission LOAD "
    if(ret.startsWith(missionLOAD)){
      var suffix = in.substring(missionLOAD.length)
      
      suffix = suffix.replace('\\', '/')

      while(suffix.contains("//")) suffix = suffix.replace("//", "/")
      while(suffix.startsWith("/")) suffix = suffix substring 1
      
      ret = missionLOAD + suffix
      
      
    }
   
   
	  ret
	} 
 
 
 
}
