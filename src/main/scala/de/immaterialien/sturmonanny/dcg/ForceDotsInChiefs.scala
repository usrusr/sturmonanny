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
  	
  	override lazy val interestingBlocks: Parser[Kept] = {
      (iniLine("Chiefs") ~ rep( chiefDefinition | brokenChiefDefinition ) ) ^^^  kept
    }
  	
  	lazy val chiefDefinition = 
  		o ~> (
  		"""[^\[\s]\S*[ \t]+"""+
  		"""[^\.\s]+\.\S+"""+ // nondot-dot-any
  		"""(?:[ \t]+-?\d+(?:\.\d*)?)+"""
  		).r ^^ keep
//  	lazy val brokenChiefDefinition = chiefDefinition 
  	/* 
128_Chief ArmorM4A2_US 1  1	  -1.0
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
  }
}