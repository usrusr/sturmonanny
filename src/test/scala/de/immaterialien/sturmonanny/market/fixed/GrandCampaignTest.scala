package de.immaterialien.sturmonanny.market.fixed

import de.immaterialien.sturmonanny.core._
import org.junit.Test
import org.junit.Assert._

class GrandCampaignTest {

	@Test 
	def testFile() : Unit ={ 
		val m = new GrandCampaignMarket
		
		m.setConfiguration("src/test/resources/de/immaterialien/sturmonanny/market/fixed/grandcampaign.price")
//		m.setConfiguration("src/test/resources/de/immaterialien/sturmonanny/market/fixed/stgrad.price")
		m.cycle(new java.io.File("src/test/resources/de/immaterialien/sturmonanny/market/fixed/italy_43194308120.mis"))
println("created "+m.priceListParser .timeTable )			
	
		def loadRed(plane:String)={
			val ret = m.tryPrice(IMarket.Loadout(plane,None), 1)
println("from red  "+plane+": "+ret )			
			ret
		}
		def loadBlue(plane:String)={
			val ret = m.tryPrice(IMarket.Loadout(plane,None), 2)
println("from blue "+plane+": "+ret )			
			ret
		}
//		loadRed("Fw-190A-5")
		loadBlue("Fw-190A-5")
	} 
	
	@Test 
	def testParser() : Unit ={
		val m = new GrandCampaignMarket
		
//		m.setConfiguration("src/test/resources/de/immaterialien/sturmonanny/market/fixed/grandcampaign.price")
		m.setConfiguration("src/test/resources/de/immaterialien/sturmonanny/market/fixed/stgrad.price")
		m.priceListParser.checkUpdate
		println("parsed: \n"+m.priceListParser)

	}
	
	@Test
	def testChangeFileTimes {
		//val fn = "./Pilot.usrusr.log"
		val args=List("", "./Pilot.usrusr.log")
		val fn = args(1)
		val minutes = 120
		val f= new java.io.File(fn)
		//println(""+new java.io.File(f).list.mkString("\n"))
		val (list, parent) = if(f.isDirectory) {
			println("backdating by "+minutes+" minutes all files in "+f.getCanonicalPath)			
			(f.list.toList.map(c=>new java.io.File(f,c)), f)
		}else {
			println("backdating by "+minutes+" minutes "+f.getCanonicalPath)			
			(List(f), f.getParent)
		}
		
		val diff = 1000*60* minutes
		var i = 0
		for(file<-list){
			if( ! file.isDirectory) {
				file.setLastModified(file.lastModified-diff)
				i+=1
			}
		}
//		new java.io.FileOu(parent, ("backdated."+minutes+"."+((new java.util.Date).toLocaleString)))
		println("done backdating for "+i+" files")
	}
}