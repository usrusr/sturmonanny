package de.immaterialien.qlmap

import javax.imageio.ImageIO

object renderTest {
  def main(args: Array[String]): Unit = {
    var mis = "src/test/resources/Italien_43194309120.mis"
    mis = "src/test/resources/Italien_43194309130.mis"
      
    val mapBase = new java.io.File("src/main/resources/mapbase")
      
    val base = new MapBase(mapBase)
    val p = new MisParser(new java.io.File(mis), base, new GroundClasses("C:/Users/ulf/Desktop/fbdj"))
    println("parese: " + p.parseResult)
    val xml = (<html>
                 <body>
                   { p.out.toHtml }
                 </body>
               </html>)

    val fw = new java.io.FileWriter("test.html")
    fw.write(xml.toString)

    //  p.out.display

    fw.close

    val in = ImageIO.read(new java.io.File("src/test/resources/Italy_Online.jpg"))
    p.out.paint(in)

    ImageIO.write(in, "JPG", new java.io.File("src/test/resources/Italy_Online.out.jpg"))

    ()
  }
}
