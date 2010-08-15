package de.immaterialien.qlmap

import scala.xml._

//import scala.swing
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io
import scala.collection._

class MisModel {
  var front: List[(Double, Double, Int)] = Nil
  var rfront: List[(Double, Double)] = Nil
  var bfront: List[(Double, Double)] = Nil
  var width = 160000.
  var height = 160000.
  var widthOffset = 0.
  var heightOffset = 0.
  private var _imageFile: io.File = null
  def frontMarker(x: Double, y: Double, a: Int) {
//    println("frontmarker: " + x + " / " + y + " for " + a)
    front = (x, y, a) :: front
    if (a == 1) rfront = (x, y) :: rfront
    if (a == 2) bfront = (x, y) :: bfront
  }
  def bornPlace(a: Integer, x: Double, y: Double) {}
  def baseInfo(baseInfo: Option[MapInfo]) {
    for (in <- baseInfo) {
      _imageFile = in.image.get
      width = in.width.getOrElse(10.) * 10000.
      height = in.height.getOrElse(10.) * 10000.
      widthOffset = in.widthOffset.getOrElse(0.) * 10000.
      heightOffset = in.heightOffset.getOrElse(0.) * 10000.
    }
  }
  def imageFile =  if(_imageFile.exists) new java.io.FileInputStream(_imageFile) else {
    this.getClass.getResourceAsStream("/mapbase/"+_imageFile.getName)
  }
  
  var rawAirFields:List[(Double, Double, Int)]=Nil
  def airfield(side:Int, x:Double, y:Double) {
    rawAirFields = (x, y, side) :: rawAirFields
  }
  var rawGroundUnits:List[(GroundClass.GC, Double, Double, Int)]=Nil
  def groundUnit(cls:GroundClass.Value, side:Int, x:Double, y:Double) {
    rawGroundUnits = (cls.asInstanceOf[GroundClass.GC], x, y, side) :: rawGroundUnits
  }
}
