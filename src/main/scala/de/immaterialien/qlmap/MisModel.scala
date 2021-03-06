package de.immaterialien.qlmap

import scala.xml._


//import scala.swing
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io
import scala.collection._

object MisModel {
	case class Waypoint(x:Double , y:Double)
	case class WingGroundAttack(override val x:Double, override val y:Double) extends Waypoint(x,y)
	case class WingTakeoff(override val x:Double, override val y:Double) extends Waypoint(x,y)
	case class WingLanding(override val x:Double, override val y:Double) extends Waypoint(x,y)
	implicit def wpToPair(wp:Waypoint):(Double,Double)=(wp.x,wp.y)
}
class MisModel {import MisModel._
  var front: List[(Double, Double, Int)] = Nil
  var rfront: List[(Double, Double)] = Nil
  var bfront: List[(Double, Double)] = Nil
  var width = 160000.
  var height = 160000.
  var widthOffset = 0.
  var heightOffset = 0.
  class Chief(
  	var name:String="", 
    var side:Option[Int]=None,
    var cls : GroundClass.GC = GroundClass.Unidentified,
    var path:List[(Double, Double)]=Nil,
    var count :Int=1
  ){  
    def classSideClassName(cls : GroundClass.GC, side:Int, className:String){
      this.side = Some(side)
      this.cls = cls
      
      count = 1 *(className match {
        case MisRender.leadingDigits(nums) => math.max(nums.toInt, 1)
        case MisRender.containsColumn() => 5
        case MisRender.containsTrain() => 10
        case _ => 1
      })
      
    }
    def waypoint(xy:(Double,Double)){
      path = path ::: xy ::Nil 
    }
  }
  val chiefs = new mutable.HashMap[String, Chief]{
    override def default(name:String):Chief={
      val ret = new Chief(name)
      put(name, ret)
      ret
    }
  }

  
  class Wing(
  	var name:String="", 
    var side:Option[Int]=None,
    var path:List[Waypoint]=Nil,
    var count :Int=1
  ){  
    def waypoint(xy:Waypoint){
      path = path ::: xy ::Nil
    }
    override def toString = "wing:"+name+" on "+side +"("+path.size+")"
    
    def side(func:((Double,Double))=>Int):Option[Int]= if(side.isDefined) side else {
    	side = path.filter(_ match{
    		case _ : WingLanding => true
    		case _ : WingTakeoff => true
    		case _ => false
    	}).headOption.map(wp => func(wp)) 
    	side
    }
  }
  val wings = new mutable.HashMap[String, Wing]{
    override def default(name:String):Wing={
      val ret = new Wing(name)
      put(name, ret)
      ret
    }
  }
  
  private var _imageFile: io.File = null
  def frontMarker(x: Double, y: Double, a: Int) {
//    println("frontmarker: " + x + " / " + y + " for " + a)
    front = (x, y, a) :: front
    if (a == 1) rfront = (x, y) :: rfront
    if (a == 2) bfront = (x, y) :: bfront
  }
  def bornPlace(a: Int, x: Double, y: Double) {}
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
    val ret = this.getClass.getResourceAsStream("/mapbase/"+_imageFile.getName)
    if(ret==null){
    	throw new java.io.FileNotFoundException("no map image "+_imageFile.getName+" found in "+_imageFile.getParentFile.getAbsolutePath)
    }
    ret
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
