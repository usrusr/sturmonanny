package de.immaterialien.sturmonanny.dcg


import de.immaterialien.sturmonanny
import scala.util.parsing.combinator._
import sturmonanny.util._
import java.io._
import scala.collection._


/**
 * 
 * problem: cases have been documented where DCG changes a line  
 *  <br/><tt>1_Chief Armor.1-M4A2_US 1</tt>
 *   
 * to
 *  <br/><tt>1_Chief ArmorM4A2_US 1</tt>
 *  
 * this object does not exist, can't be created by the game and can't be destroyed in the log
 * 
 * <p>solution: change it into something that can be destroyed (will it wear multi-tank columns down, over the course of many missions?)
 * 
 * @author ulf
 *
 */
class ForceDotsInChiefs (args: String) extends DoNothingMisRewriter(args){
	
	override def gatherer = new ForceDotsInChiefs.Gatherer()
}

object ForceDotsInChiefs {

	
  /**
   * will make one pass 
   */
  protected class Gatherer extends DoNothingMisRewriter.Gatherer {
  	
  	override lazy val interestingBlocks: Parser[Kept] = {(
      ((iniLine("Chiefs") ~ rep( 
      		  chiefDefinition 
      		| brokenChiefDefinition
      		) ) ^^^  kept)
      | ((iniLine("NStationary") ~ rep( 
      		  nStationary
      		| brokenNStationary
      		) ) ^^^  kept)
    )}
  	
  	lazy val chiefDefinition = 
  		o ~> (
  		"""[^\[\s]\S*[ \t]+"""+
  		"""[^\.\s]+\.\S+"""+ // nondot-dot-any
  		"""(?:[ \t]+-?\d+(?:\.\d*)?)+"""
  		).r ^^ keep
//  	lazy val brokenChiefDefinition = chiefDefinition 
  	/*
  	 * handles this error:
128_Chief ArmorM4A2_US 1  1	  -1.0
23_Chief2 vehicles.artillery.Artillery$23_Chief ArmorPzIVE 2 23587.00 101912.00 423.0 0.0 0
  	 */
  	lazy val brokenChiefDefinition = o ~> matcher(
  			"""([^\[\s]\S*[ \t]+)"""+ // prefix: name and blanks
  			"""[^\.\s]+"""+ // dotless type
  			"""((?:[ \t]+-?\d+(?:\.\d*)?)+)"""
  			) ^^ { case groups =>
  		keep(groups.group(1))
  		keep("Vehicles.Bicycle")
  		keep(groups.group(2))
  	}
  	
  	lazy val nStationary = o ~> matcher(
  			"""([^\[\s]\S*[^ \t\d])(\d*)[ \t]+"""+ // prefix: name and blanks
  			"""([^\s\$]+\$[^\s]+)"""+ // type$chief 
  			"""((?:[ \t]+-?\d+(?:\.\d*)?)+)"""
  			
//  			"""([^\[\s]\S*[ \t]+)([^\s\$]+\$)\1[ \t]+([^\s\d]+)((?:[ \t]+-?\d+(?:\.\d*)?)+)"""  			
  			) ^^ { case groups => 
 			
  		keep(groups.group(1)) // 23_Chief
  		keep(groups.group(2)) // 2
  		keep(groups.group(3)) // vehicles.artillery.Artillery$ArmorPzIVE
  		keep(groups.group(4)) //  2 23587.00 101912.00 423.0 0.0 0
  		
//  		keep("Vehicles.Bicycle")
//  		keep(groups.group(2))
  	}
  	
  	
  	/* 
this error:
23_Chief2 vehicles.artillery.Artillery$23_Chief ArmorPzIVE 2 23587.00 101912.00 423.0 0.0 0
  	 */
  	lazy val brokenNStationary = o ~> matcher(
  			"""([^\[\s]\S*[^ \t\d])(\d*)[ \t]+"""+ // prefix: name and blanks
  			"""([^\s\$]+\$)\1[ \t]+([^\s\d]+)"""+ // type$chief [blank] [nonnum] type
  			"""((?:[ \t]+-?\d+(?:\.\d*)?)+)"""
  			
//  			"""([^\[\s]\S*[ \t]+)([^\s\$]+\$)\1[ \t]+([^\s\d]+)((?:[ \t]+-?\d+(?:\.\d*)?)+)"""  			
  			) ^^ { case groups => 
 			
  		keep(groups.group(1)) // 23_Chief
  		keep(groups.group(2)) // 2
  		keep(groups.group(3)) // vehicles.artillery.Artillery$
  		keep(groups.group(4)) // ArmorPzIVE
  		keep(groups.group(5)) //  2 23587.00 101912.00 423.0 0.0 0
  		
//  		keep("Vehicles.Bicycle")
//  		keep(groups.group(2))
  	}
  }
}