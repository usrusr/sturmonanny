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
  var imageFile: io.File = null
  def frontMarker(x: Double, y: Double, a: Int) {
//    println("frontmarker: " + x + " / " + y + " for " + a)
    front = (x, y, a) :: front
    if (a == 1) rfront = (x, y) :: rfront
    if (a == 2) bfront = (x, y) :: bfront
  }
  def bornPlace(a: Integer, x: Double, y: Double) {}
  def baseInfo(baseInfo: Option[MapInfo]) {
    for (in <- baseInfo) {
      imageFile = in.image.get
      width = in.width.getOrElse(10.) * 10000.
      height = in.height.getOrElse(10.) * 10000.
      widthOffset = in.widthOffset.getOrElse(0.) * 10000.
      heightOffset = in.heightOffset.getOrElse(0.) * 10000.
    }

  }
  var rawGroundUnits:List[(GroundClass.GC, Double, Double, Int)]=Nil
  def groundUnit(cls:GroundClass.GC, side:Int, x:Double, y:Double) {
    rawGroundUnits = (cls, x, y, side) :: rawGroundUnits
  }
 

//  def toHtml(): NodeSeq = {
//    var out =
//      <div style="position:inherit; float:left; border: thick; border-color: red; border-style: solid">
//        <div style="position: relative; width: 100%; display: block;">
//          <img src="src/test/resources/Italy_Online.jpg" style="size: 100%; display: block;"/>
//          { front map frontMarkers }
//          { htmlFront }
//        </div>
//      </div>
//    for (f <- front) {}
//    out
//  }
//
//  def htmlPos(x: Double, y: Double): Node = {
//    val r = sideVal(x, y, rfront)
//    val b = sideVal(x, y, bfront)
//    val color = { if (r > b) "red" else "blue" }
//    val singleMagnitude = 100 * { if (r > b) b else r } / (width * height)
//    val magnitude = 1 * Math.sqrt(singleMagnitude)
//    (<div position="relative" style={
//      "display: block; float: left; position:absolute; top: " + (100 * (1 - y)).toInt + "%; left:" + (100 * x).toInt + "%"
//    }><div style={ "position:relative;top:-1ex;left:-1ex;color:" + color + ";" }>{ magnitude.toInt }</div></div>)
//  }
//
//  def htmlFront: NodeSeq = {
//    val count = 75
//    for {
//      x <- 0 to count
//      y <- 0 to count
//    } yield htmlPos(x.toDouble / count, y.toDouble / count)
//  }
//
//  var i = 0;
//  def frontMarkers(pair: (Double, Double, Int)): NodeSeq = {
//    <div position="relative" style={
//      "display: block; float: left; position:absolute; top: " + (100 - 100 * pair._2 / width).toInt + "%; left:" + (100 * pair._1 / height).toInt + "%"
//    }>{ pair._3 }:{ i = i + 1; i }</div>
//  }
}
