package de.immaterialien.qlmap.sprites

import de.immaterialien.sturmonanny.util
import de.immaterialien.qlmap._
import java.awt.image._
import javax.imageio.ImageIO
import GroundClass._
import scala.collection._

object Sprites extends util.Log{ 
  val map = new mutable.HashMap[String, Option[BufferedImage]]
  def forClass(cls:GC, side:Int):Option[BufferedImage]={
    val infix = if(side==1) ".Red" else if(side==2) ".Blue" else ""
    val fname : String = (cls match {
      case Artillery => Car.toString
      case Plane => "Airfield"
      case x => x toString
    })+infix+".png"
    
    val ret = map.get(fname).getOrElse{
println("getting "+fname)
//Sprites.getClass.classPathResource(fname)
      val stram = Sprites.getClass.getResourceAsStream("/de/immaterialien/qlmap/sprites/"+fname)
println("stream "+stram)      
      val iio = try {
        Some(ImageIO.read(stram))
      }catch{
        case _ => {
          log.debug("not found: "+fname)
          None
        }
      }
      map put (fname, iio)
      iio
    }
println("got "+fname+"->"+ret)      
    ret
  }
}