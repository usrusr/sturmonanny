package de.immaterialien.qlmap

object renderAll {
  def main(args : Array[String]) : Unit = {}
  
  
  
  def render(what:File){
    var mis = "src/test/resources/Italien_43194309120.mis"
    mis = "src/test/resources/Italien_43194309130.mis" 
    val p = new MisParser(new java.io.File(mis), null)
println("parese: "+p.parseResult)
	val xml = (<html><body>
 {p.out.toHtml}
 </body>
 </html>)

 	
 	val fw = new java.io.FileWriter("test.html")
  fw.write(xml.toString)
  
//  p.out.display
  
  fw.close
  
  val in = ImageIO.read(new java.io.File("src/test/resources/Italy_Online.jpg"))
  p.out.paint(in)
  
  ImageIO.write(in, "JPG", new java.io.File("src/test/resources/Italy_Online.out.jpg"))
      
  }
}
