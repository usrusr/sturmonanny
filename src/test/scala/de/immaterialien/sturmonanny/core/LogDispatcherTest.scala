package de.immaterialien.sturmonanny.core

import org.junit.Test
import org.junit.Assert._
import scala.util.parsing.combinator._

import scala.io

object LogDispatcherTest {
  def main(args : Array[String]) {
    new LogDispatcherTest().test
  }
}
class LogDispatcherTest { 
  val dispatch = new EventLogDispatcher() { 
	  override def pilotNameParser : Parser[String] = {   
	  		"blackjack_89"|"leon1"|"Charly-Lima"|"Seraphim"|"ACX"|"AIST"|"Apostol"|"usrusr"
	  }
	  override def learnNewName(pilotName : String):Unit = println(" would have learned '"+pilotName+"'")
  }
  def one(line : String) {
    val res = dispatch testOneLine line
    println("..-parsing: "+res+" <- '"+line+"'")
  } 
  def test(p: dispatch.Parser[_], in:String) {
  	val r = dispatch.parse(p, in)
  	println(r+" "+r.getClass.getCanonicalName+"<-- "+in)
  }

    @Test
  def test2() : Unit = {
    println("starting")
    one("[Jun 9, 2010 1:32:33 PM] Mission BEGIN")
    one("[3:45:32 PM] usrusr:Fw-190F-8(0) bailed out at 78692.71 200178.1")    
  }
  
  @Test
  def test() : Unit = {
    println("starting")
 


    println("...")
    one("[Jun 9, 2010 6:32:33 PM] Mission BEGIN")
    one("[1:45:32 PM] usrusr:Fw-190F-8(0) bailed out at 78692.71 200178.1")
//    one("[Jun 9, 2010 6:32:33 PM] Mission BEGIN")
//    one("[6:32:33 PM] Mission BEGIN")
    
    test(dispatch.atLocationParser, " at 73896.164 72082.55")
    test(dispatch.pilotNameParser, "blackjack_89")
test(dispatch.fuelParser, "fuel 50%")
test(dispatch.loading, "blackjack_89:Ju-88A-4 loaded weapons '28xSC50_2xSC250' fuel 50%")
    
    one("[6:37:56 PM] blackjack_89:Ju-88A-4(0) seat occupied by blackjack_89 at 73896.164 72082.55")
    one("[6:37:57 PM] blackjack_89:Ju-88A-4 loaded weapons '28xSC50_2xSC250' fuel 50%")

//    val fUrl = this.getClass.getResource("Afrika_42194205120-log.txt")
//    println("using "+fUrl)
//    val stream = this.getClass.getResourceAsStream("Afrika_42194205120-log.txt")
    val stream = this.getClass.getResourceAsStream("bailout.log")
    val source = io.Source.fromInputStream(stream)
//
    for (line <- source.getLines) {
      one(line)
    }

    println(".done.")
  }
}