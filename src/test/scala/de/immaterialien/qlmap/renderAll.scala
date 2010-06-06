package de.immaterialien.qlmap
import java.io

object renderAll {
	val pat = (""".*\Q.mis\E""").r
   object dotMis extends io.FilenameFilter {
    
//    override def accept(path:io.File, fname:String):Boolean = fname match {
//      case pat(_) => true
//      case _ => false
//    }
    override def accept(path:io.File, fname:String):Boolean = pat.unapplySeq(fname).isDefined
    
  } 
  def main(args : Array[String]) : Unit = {
    val misPath = new io.File("src/test/resources")
    val mapBase = new io.File("src/main/resources/mapbase")
    
    var pat = ".*".r
//    pat = "Berlin.*".r
    	pat = "Iasi.*".r
//    pat = "Afrika_42194204050.*".r 
//    pat = "Afrika.*".r 
    
    val base = new MapBase(mapBase)
    
    val list = misPath.list(dotMis)
    
    for(mis<-list.filter(pat.unapplySeq(_).isDefined)) {
      val pars = new MisParser(new java.io.File(misPath, mis), base)
      val img = pars.out.paint(new io.File(misPath, mis))
      
      
      ()
    }
    
    ()
  }
  
  
  
  
     
}
