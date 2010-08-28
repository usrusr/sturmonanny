package de.immaterialien.qlmap.sprites
import java.io._

import de.immaterialien.qlmap._
import java.awt.image._
import javax.imageio.ImageIO
import GroundClass._
import scala.collection._
import java.awt.geom._


class Sprites(mapBaseFolder:Option[File]) extends Log{ 
  val map = new mutable.HashMap[String, Option[Paintable]]

  def paintable(cls:GC, side:Int):Option[Paintable]={
    val key=""+cls+side 
    map.get(key).getOrElse{
      val ret = memPaintable(cls, side )
      map.put(key, ret)
      ret
    }
  }
  def memPaintable(cls:GC, side:Int):Option[Paintable]={
    
    val scalable = ScalableSprite.create(cls, side, mapBaseFolder)
    scalable.map(x=>Some(x)).getOrElse(
      BitmapSprite.create(cls, side)
    )
  }
  
}
