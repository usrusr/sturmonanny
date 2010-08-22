package de.immaterialien.qlmap
import java.io

object RenderAll {
  val pat = (""".*\Q.mis\E""").r
  object dotMis extends io.FilenameFilter {
 
    //    override def accept(path:io.File, fname:String):Boolean = fname match {
    //      case pat(_) => true
    //      case _ => false 
    //    }
    override def accept(path: io.File, fname: String): Boolean = pat.unapplySeq(fname).isDefined

  }  
  def main(args: Array[String]): Unit = {
    val misPath = new io.File("src/test/resources")
    val mapBase = new io.File("__src/main/resources/mapbase")

    var pat = ".*".r
//        pat = "Berlin.*".r
//    pat = "Dese.*".r
//    pat = "Berlin_45194503220.*".r
//    pat = "Iasi.*".r
    pat="Iasi44194405020.*".r
//    pat="Italien_43194309120.*".r
//        pat = "Afrika_42194204050.*".r 
//        pat = "Afrika.*".r 

    val base = new MapBase(mapBase)

    val list = misPath.list(dotMis)

    for (mis <- list.filter(pat.unapplySeq(_).isDefined)) {
      val pars = new MisParser(new java.io.File(misPath, mis), base, new GroundClasses("C:/Users/ulf/Desktop/fbdj__"))
      val before = System.currentTimeMillis
      val img = MisRender.paint(new io.File(misPath, mis), pars.out, base)
//      val img = pars.out.paint(new io.File(misPath, mis))
println("painted in "+(System.currentTimeMillis-before))
      
      ()
    }

    ()
  }

}
