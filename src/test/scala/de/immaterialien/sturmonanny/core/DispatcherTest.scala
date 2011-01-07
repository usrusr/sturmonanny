package de.immaterialien.sturmonanny.core

import org.junit.Test
import org.junit.Assert._


class DispatcherTest {
	net.lag.configgy.Configgy.configure("log.conf")
//	@Test
	 def direct() : Unit = {
//	  val r = """Chat: --- (.+) (?:(:?was killed)|(?:has crashed)|(?:bailed out))\.\\n""".r
	   val r = """Chat: --- (.+) was killed|has crashed|bailed out\.\\n""".r
	  assertTrue("""Chat: --- entrop regulation was killed.\n""" match {
	    case r(who) => true
     case _ => false
	  })
	 }
	@Test  
	def testRegex() : Unit ={
		val d = new LocalizedDispatcher(){
		  override def conf = new Configuration("default.conf", null)
		  override def pilotMessageSend(who:String, what:Is.Event) = debug("would have sent to "+who+"<-"+what) 
		} 
		d.updateConfiguration
  
  		d.pilotNameParser.add("1") // d.pilotNameParser learnNewName "1"
  		d.pilotNameParser.add("2") // d.pilotNameParser learnNewName "2"
  		d.pilotNameParser.add("3") // d.pilotNameParser learnNewName "3"
//Thread sleep 30      
  		d.pilotNameParser.add("4") // d.pilotNameParser learnNewName "4"
  		d.pilotNameParser.add("5") // d.pilotNameParser learnNewName "5" 
  		d.pilotNameParser.add("6") // d.pilotNameParser learnNewName "6" 
//  		d.pilotNameParser.add("I./NJG6_Messer")
//  		d.pilotNameParser.add("II./ZG26-Ruhland")
  		
  		
d.pilotNameParser.add("OTAMAH")
d.pilotNameParser.add("JG333_Siegie")
d.pilotNameParser.add("88.IAP>Wolle")
d.pilotNameParser.add("*BG*UVIGI")
d.pilotNameParser.add("FG28_ERASER")
d.pilotNameParser.add("viliuzz")
d.pilotNameParser.add("^GNS^Sh@kal")
d.pilotNameParser.add("88.IAP_Sokol")
d.pilotNameParser.add("Red08")
d.pilotNameParser.add("stefanP007")
d.pilotNameParser.add("Varrattu")
d.pilotNameParser.add("ZG15_Axel")
d.pilotNameParser.add("I/JG11_Kalle")
d.pilotNameParser.add("EAF602Redundant")
d.pilotNameParser.add("Terrible")
d.pilotNameParser.add("usrusr")
d.pilotNameParser.add("USMCG_K-Bar")
d.pilotNameParser.add("entrop regulationabcdefg")
d.pilotNameParser.add("EJGr.Ost_yogy")
d.pilotNameParser.add("veis_zimerman")
d.pilotNameParser.add("Esaul_75")
d.pilotNameParser.add("ejgr.ost_Hans")
d.pilotNameParser.add("joachim67")
d.pilotNameParser.add("=Nachtigall=")
d.pilotNameParser.add("Amarok")
d.pilotNameParser.add("ESCOMM_Welter")
d.pilotNameParser.add("SLOJani")
d.pilotNameParser.add("I./ZG15_helterskelter")
d.pilotNameParser.add("SkyWalker")
d.pilotNameParser.add("ZG15_Falke")
d.pilotNameParser.add("II./ZG26-Ruhland")
d.pilotNameParser.add("EAF602Flasheart")
d.pilotNameParser.add("1./JG42_mugen")
d.pilotNameParser.add("88.IAP_Terni")
d.pilotNameParser.add("318_Dadi")
d.pilotNameParser.add("Pilus")
d.pilotNameParser.add("I./ZG15_robtek")
d.pilotNameParser.add("1/3_Kristoforo")
d.pilotNameParser.add("_ITAF_Gianper")
d.pilotNameParser.add("Jenisch")
d.pilotNameParser.add("=ESF=DannyBoy")
d.pilotNameParser.add("Kalle")
d.pilotNameParser.add("=FI=Genosse")
d.pilotNameParser.add("I./ZG15_Falke")
d.pilotNameParser.add("=ESF=Jgunner")
d.pilotNameParser.add("III/JG27*Angus")
d.pilotNameParser.add("JG27~TaZzu")
d.pilotNameParser.add("88.IAP_Andre")
d.pilotNameParser.add("manevras")
d.pilotNameParser.add("qtepees")
d.pilotNameParser.add("I./ZG15_Axel")
d.pilotNameParser.add("bdfy33")
d.pilotNameParser.add("-DS-")
d.pilotNameParser.add("LeLv76_Oke")
d.pilotNameParser.add("jmla")
d.pilotNameParser.add("ejgr.ost_Irmin")
d.pilotNameParser.add("MAD-MM")
d.pilotNameParser.add("=FI=Airway")
d.pilotNameParser.add("dengar")
d.pilotNameParser.add("jojof1")
d.pilotNameParser.add("mugen")
d.pilotNameParser.add("alex.68")
d.pilotNameParser.add("wurzel")
d.pilotNameParser.add("I./NJG6_Messer")
d.pilotNameParser.add("FOKA")
d.pilotNameParser.add("I./ZG26-Sturm")
d.pilotNameParser.add("=Cad=Apolo")
d.pilotNameParser.add("PB0_Bily")
d.pilotNameParser.add("EAF310_Thor")
d.pilotNameParser.add("XOXOL")
d.pilotNameParser.add("|450|boom")
d.pilotNameParser.add("Anw./ZG15_Rost")
d.pilotNameParser.add("EJGr.Ost_Irmin")
  		
  		
		d.pilotNameParser.add("entrop regulation") // d.pilotNameParser learnNewName "entrop regulation" 
    d processLine """Chat: --- Pilot(entrop regulation) was killed.\n"""
    
    println("direct parse result: "+ d.parseAll(d.statusChat, """Chat: --- Pilot(1) was killed.\n""")) 
    println("direct parse result newline: "+ d.parseAll(d.statusChat, """Chat: --- Pilot(2) was killed.\n""")) 

  		d.pilotNameParser.add("Mad") // d.pilotNameParser learnNewName "Mad"
//  		d.pilotNameParser.add("entrop regulationabcdefg","entrop regulationabcdefg") // d.pilotNameParser learnNewName "entrop regulationabcdefg"

    d processLine """\"""+"""u00201      entrop regulationabcdefg 1 0    (2)Blue     ZZ + AH     Ju-87D-5\n"""
    d processLine """\"""+"""u00201      entrop regulationabcdefg 1 0    (2)Blue     ZZ(9)sZa    Ju-87D-5\n"""
    
//  		d.pilotNameParser.add("Sturm0jetty","Sturm0jetty") // d.pilotNameParser learnNewName "Sturm0jetty"
  
  		d processLine """Chat: --- Pilot(entrop regulation) was killed.\n"""

  
		d processLine """Chat: Mad: \ttest msg T s\n"""
		d processLine "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
		d processLine "\\"+"""u00201       Mad            1       0       (2)Blue     < +         Bf-109G-2\n"""
//Thread sleep 2000  
		d processLine "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
		d processLine "\\"+"""u00201       Mad            1       0       (2)Blue     red-2       Porsche\n"""
		d processLine "\\"+"""u00201       Sturm0jetty    1999    2900    (1)Red      \n"""
//Thread sleep 2000  
		d processLine "\\"+"""u0020N       Name           Ping    Score   Army        Aircraft\n"""
		d processLine "\\"+"""u00201       Mad            1       0       (2)Blue     red-2       IL-2\n"""
	}
}
 