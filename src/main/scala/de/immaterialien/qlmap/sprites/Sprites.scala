package de.immaterialien.qlmap.sprites


import de.immaterialien.qlmap._
import java.awt.image._
import javax.imageio.ImageIO
import GroundClass._
import scala.collection._
import java.awt.geom._


object Sprites extends Log{ 
  val map = new mutable.HashMap[String, Option[Paintable]]

  def paintable(cls:GC, side:Int):Option[Paintable]={
    val key=""+cls+side
    map.get(key).getOrElse{
      val ret = memPaintable(cls:GC, side:Int)
      map.put(key, ret)
      ret
    }
  }
  def memPaintable(cls:GC, side:Int):Option[Paintable]={
    
    val scalable = ScalableSprite.create(cls, side)
    scalable.map(x=>Some(x)).getOrElse(
      BitmapSprite.create(cls, side)
    )
  }
  
}
