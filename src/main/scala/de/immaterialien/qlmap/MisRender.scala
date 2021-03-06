package de.immaterialien.qlmap

import javax.imageio.ImageIO
import java.awt
import awt.image._
import awt.geom._
import java.io
import scala.collection._
import de.immaterialien.qlmap.sprites.Sprites
import de.immaterialien.gfxutil.Implicits._
import MisModel._ // e.g. for the waypoint -> pair implicit

object MisRender extends Log {
  val leadingDigits = """(\d+)\D.*""".r
  val containsColumn = """.*Column.*""".r
  val containsTrain = """.*Train.*""".r
  //var debugMode = false
  def paint(forMission: io.File, model: MisModel, mapBase: MapBase, mapWriter:Option[mutable.Buffer[scala.xml.Elem]]=None): Option[io.File] = try {
    val outputPath = mapBase.configuration.flatMap(_.outPath)
    val sprites: Sprites = new Sprites(Some(mapBase.folder))
    val conf = mapBase.configuration.getOrElse(new MapConf(""))
    var format: String = mapBase.configuration.map(_.outputFormat.apply).getOrElse("JPG")

    //     reanimate to enable PNG output...    
    //         val iis = ImageIO.createImageInputStream(model.imageFile)
    //         val readers = ImageIO.getImageReaders(iis)
    //         while(format==null && readers.hasNext) {
    //           val reader = readers.next
    //           format = reader.getFormatName 
    //         }
    //         iis.close

    // jpg
//    if (format == null) format = "JPG"
    //format = "PNG"
//format  = "GIF"
    var instream: java.io.InputStream = null
    var ig: java.awt.Graphics2D = null
    try {
      instream = model.imageFile
      if(instream==null){
      	log.error("no instream for ")
      }
      var in = ImageIO.read(instream)
      ig = in.createGraphics()
      new MisRender(
      	conf,
        model,
        ig,
        sprites,
        in.getHeight,
        in.getWidth,
        mapWriter
        ).sequence(in)
      val outPath = outputPath.getOrElse(forMission.getParentFile)
      val outFile = new java.io.File(outPath, forMission.getName + "." + format)
      ImageIO.write(in, format, outFile)
      Some(outFile)
    } finally {
      if(instream!=null) try instream.close
      if(ig!=null) try ig.dispose
    }
  } catch {
    case x => {
      log.error("failed to paint for " + forMission, x)
      None
    }
  }
}

private class MisRender(
	conf:MapConf,
  model: MisModel,
  ig2: java.awt.Graphics2D,
  spritesMaker: Sprites,
  ih: Int,
  iw: Int,
  mapWriter:Option[mutable.Buffer[scala.xml.Elem]]
  ) {
  import MisRender._

  val randomize = true && false
  val debugMode = conf.debug.apply
  val chiefDepth = conf.chiefs.depth.apply
  val unitDepth = conf.units.depth.apply
  val interpolate = conf.front.interpolate.apply
  var xsvr = iw / interpolate
  var ysvr = ih / interpolate
  lazy val svrs = {
    val xsvr = iw
    Array.tabulate(2, xsvr + 2, ysvr + 2) { (_, _, _) =>
      Double.NaN
    }
    
//        val aaa : Array[Array[Array[Double]]]= Array.tabulate(2, xsvr + 2, ysvr + 2) { _:(Int, Int, Int) =>
//      Double.NaN
//    }
//    aaa
  }
  //  val svr = 100
  //  val svrs = Array.fromFunction { (_, _, _) =>
  //    Double.NaN
  //  }(2, svr + 2, svr + 2)

  /**
   * 
   * @param x 0..1
   * @param y 0..1
   * @param markers
   * @return
   */
  def sideVal(x: Double, y: Double, markers: List[(Double, Double)]): Double = {import math._  
  	sideValWithLimits(max(0D, min(1D, x)), max(0D, min(1D, y)), markers) 
  }
  private def sideValWithLimits(x: Double, y: Double, markers: List[(Double, Double)]): Double = {
    val sideIndex = if (markers eq model.rfront) 0 else 1
    //println("x:"+x+" y:"+y)    
    def buffered(ix: Int, iy: Int): Double = {

      val ret = svrs(sideIndex)(ix)(iy)
      //if(ret!=Double.NaN) ret else {
      if (!ret.isNaN) ret else {
        var tmp = preciseSideVal(ix.toDouble / xsvr.toDouble, iy.toDouble / ysvr.toDouble, markers)
        svrs(sideIndex)(ix)(iy) = tmp
        tmp
      }
    }

    val ix = math.floor(x * xsvr).toInt
    val iy = math.floor(y * ysvr).toInt
    var ww = {
      val ll = buffered(ix, iy)
      val hl = buffered(ix + 1, iy)
      val lh = buffered(ix, iy + 1)
      val hh = buffered(ix + 1, iy + 1)

      def interpolate(c1: Double, c2: Double, c: Double, v1: Double, v2: Double): Double = {
        val (lc, hc, lv, hv) = if (c1 < c2) (c1, c2, v1, v2) else (c2, c1, v2, v1)
        val dist = hc - lc
        ((c - lc) * hv + (hc - c) * lv) / dist
      }
      //    val wl = interpolate(ix.toDouble/svr.toDouble, (ix+1).toDouble/svr.toDouble, x, ll, hl) 
      //    val wh = interpolate(ix.toDouble/svr.toDouble, (ix+1).toDouble/svr.toDouble, x, lh, hh) 
      //    
      //    val ww = interpolate(iy.toDouble/svr.toDouble, (iy+1).toDouble/svr.toDouble, y, wh, wh)
      //    
      val lw = interpolate(iy.toDouble / ysvr.toDouble, (iy + 1).toDouble / ysvr.toDouble, y, ll, lh)
      val hw = interpolate(iy.toDouble / ysvr.toDouble, (iy + 1).toDouble / ysvr.toDouble, y, hl, hh)

      interpolate(ix.toDouble / xsvr.toDouble, (ix + 1).toDouble / xsvr.toDouble, x, lw, hw)
    }
    ww
  }
  /**
   * 
   * @param x 0..1
   * @param y 0..1
   * @param markers in game coords
   * @return
   */
  def preciseSideVal(x: Double, y: Double, markers: List[(Double, Double)]): Double = {
    val zero: (Double, Double) = (0, 0)
    val ret = markers.foldLeft(zero)(((acc: (Double, Double), coord: (Double, Double)) => {
      val (px, py) = coord
      val xd = (x * model.width - (px - model.widthOffset))
      val yd = (y * model.height - (py - model.heightOffset))
      val dist = math.sqrt(xd * xd + yd * yd)
      val dimension = model.width + model.height
      val add = (math.pow(dimension / dist, 3)) / dist
      //       val add = dimension*dimension*dimension/(dist*dist*dist*dist)
      //       val add = dimension*dimension/ (dist*dist*dist) 
      (acc._1 + add, math.max(acc._2, add))
    }))

    (ret._1 / markers.size) * 1 + ret._2
  }

  def sequence(in: BufferedImage) {
    if(debugMode)veil()
//    veil()
	  hatch(conf.front.hatchdistance.apply) 
	  front(conf.front.subdivisions.apply, conf.front.interpolate.apply)
////  	         front(conf.front.subdivisions.apply, 1)
	  nstatics
    forChiefs
//    forSide(1)
//    forSide(2)
    
    
    airfields()
  	wings()
  }
  
  def sideForGame(gameCoords:(Double, Double)):Int={
  	val gxy = gameToRelative(gameCoords)
  	val (who, _ ) = whoAndDeepness(gxy._1, gxy._2, 10000)
  	who
  }
  
  def wings() {
  	// init side if possible (assumes that takoff or landing is within friendly territory)
  	for((name, wing)<-model.wings) wing side (sideForGame _)

  	
  	if(debugMode){
  		ig2.setColor(awt.Color.cyan)
  		for((name, wing)<-model.wings){
  			ig2.setColor(awt.Color.cyan)
  			for(side <- wing.side){
  				
  				if(side==1) ig2.setColor(awt.Color.red)
  				else ig2.setColor(awt.Color.blue)
  			}
  			
  			if(wing.path.size>0){
	  			var last=gameToImage(wing.path.head)
	  			for(next<-wing.path.tail.map(x => gameToImage(x))){
	  				ig2.drawLine(last._1,last._2,next._1,next._2) 
	  				last=next
	  			}
  			}
  		}
  		
  	}
  	
  	for((name, wing)<-model.wings){
println("wing "+name)  		
  		for(wp<-wing.path) wp match {
	  		case gattack : WingGroundAttack => {
	  			if(debugMode){
	  				ig2.setColor(awt.Color.cyan)
						for(side <- wing.side){
							if(side==1) ig2.setColor(awt.Color.red)
							else ig2.setColor(awt.Color.blue)
						}  				
	  				val(gx, gy) =gameToImage(gattack)
	  				val radius = 9
	  				ig2.drawOval(gx-radius, gy-radius,(2*radius),(2*radius))
	//  				if()
	  			}
	  			if(Math.random < (conf.wings.probabilities.groundAttack.apply.toDouble / 100D)) {
	  				
	  				val (px, py) = gameToImage(gattack)
println("known gattack at "+px+" "+py)	  				
	  				for(side <- wing.side){
	  					drawObject(px, py, 1, side, 1, GroundClass.WingBomb)
	  				}
	  			}else{
println("surprise gattack")	  				
	  			}
	  		}
	  		case _ => 
	  	}
	  }
  }
  def veil() {

    val darkness = 30
    val light = 255 - darkness
    val background = 120 // higher -> more base image

    ig2.setColor(new java.awt.Color(light, light, light, 255 - background))
    ig2.fillRect(0, 0, iw, ih)
  }

  
  def gameToImage(xy:(Double, Double)):(Int, Int)={
  	val ix:Double=xy._1
  	val iy:Double=xy._2
  	val finalX = ((((ix) - model.widthOffset) / model.width) * iw).toInt
    val finalY = ih - (((((iy) - model.heightOffset) / model.height) * ih)).toInt
    (finalX, finalY)
  }
  
  def gameToRelative(xy:(Double, Double)):(Double,Double) = {
  	    val (x, y) = (xy._1, xy._2)
        val ox = x - model.widthOffset
        val oy = y - model.heightOffset
        val rx = ox / model.width
        val ry = oy / model.height
        
        (rx, ry)
  }
  
  def airfields() {
    for ((gx, gy, side) <- model.rawAirFields) {
//      val ox = gx - model.widthOffset
//      val oy = gy - model.heightOffset
//      val rx = ox / model.width
//      val ry = oy / model.height
//      val px = rx * iw
//      val py = ih - ry * ih

    	val (px,py)=gameToImage(gx, gy)
    	
      drawObject(px.toInt, py.toInt, 1, side, 1, GroundClass.Airfield)
    }
  }
  private def calculateScale(number:Int)={
    val c = 0.20+(math.log(number)*0.12)
//    var s = 0.3
//    var d = number.toDouble
//    while (d / 2 > 2) {
//      s += 0.08
//      d = d / 2
//    }
//println("iterative scale: "+s +" vs. "+c)    
//    s
    c
  }
//  def units() {
    /**
     * returns class, weight, count and relative position of dominant class
     */
    def atMarker(mx: Double, my: Double, mside: Int, radiusFactor:Double): Option[(GroundClass.GC, Double, Int, (Int, Int))] = {
      val radius:Double = conf.units.groundRadius.apply.toDouble * radiusFactor *0.5D
      def dist(ax: Double, ay: Double) = {
        val dx = ax - mx
        val dy = ay - my
        math.sqrt(dx * dx + dy * dy)
      }
      class Counter {
        var i = 0d
        var num = 0
        var cumulatedOffX = 0D
        var cumulatedOffY = 0D
        // in game coords
        def offX = cumulatedOffX / num
        // in game coords
        def offY = cumulatedOffY / num
        /**
         * weight within the radius is constant, drops off gently outside 
         */
        def addSoft(ax: Double, ay: Double) = {
          val update = radius / math.max(radius, dist(ax, ay))
          val countUpdate = if (radius > dist(ax, ay)) 1d else 0d
//          val countUpdate = update
          i = i + update
          num += countUpdate.toInt
          cumulatedOffX += countUpdate * (ax - mx)
          cumulatedOffY += countUpdate * (ay - my)
        }
        def addHard(ax: Double, ay: Double) = {
          val update = if (radius > dist(ax, ay)) 1d else 0d
          i = i + update
          num += update.toInt
          cumulatedOffX += update * (ax - mx)
          cumulatedOffY += update * (ay - my)
        }
        def thisOrOther(thisCls: GroundClass.GC, oCls: GroundClass.GC, oCnt: Double): (GroundClass.GC, Double) = {
          if (oCnt > i) (oCls, oCnt) else (thisCls, this.i)
        }

        override def toString = num + ":w(" + i + ")@" + offX + "," + offY
      }

      var map: mutable.Map[GroundClass.GC, Counter] = mutable.HashMap()
      for ((cls, x, y, side) <- model.rawGroundUnits; if side == mside) {
        val count = map.get(cls).getOrElse {
          val c = new Counter
          map.put(cls, c)
          c
        }
        count.addHard(x, y)
//        count.addSoft(x, y)
      }
      if (map.isEmpty) None else {
        val order = new Ordering[(GroundClass.GC, Counter)] {
          override def compare(o1: (GroundClass.GC, Counter), o2: (GroundClass.GC, Counter)) = {
            val v1 = (o1._2.i) * o1._1.weight
            val v2 = (o2._2.i) * o2._1.weight

            if (v1 > v2) 1 else if (v2 > v1) -1 else 0
          }
        }

        val isAirfield = map.get(GroundClass.Airfield).map(counter =>
          counter.num > 0
          ).getOrElse(false)

        if (isAirfield) for (fuelCount <- map.get(GroundClass.Fuel)) {
          val airfieldFuelPenalty = GroundClass.Fuel.weight / 2
          fuelCount.i = fuelCount.i / airfieldFuelPenalty
        }

        //val ret = map.max(order)
        val (cls, counter) = map.max(order)
        log debug ("  identified " + counter + " " + cls + " from " + map)
        Some(cls, counter.i, counter.num, (counter.offX.toInt, counter.offY.toInt))

      }
    }
    @deprecated
    def forSide(side: Int) {
      val markers = if (side == 1) model.rfront else model.bfront
      val other = if (side == 1) model.bfront else model.rfront
      val sd = if (side == 1) "red" else if (side == 2) "blue" else "???"
      val allMarkers = model.front.map(x=>(x._1,x._2))
//      for (marker <- markers) {
//      for (marker <- other) {
      for (marker <- allMarkers) {
        val (x, y) = (marker._1, marker._2)
//        val ox = x - model.widthOffset
//        val oy = y - model.heightOffset
//        val rx = ox / model.width
//        val ry = oy / model.height
      	val (rx, ry) = gameToRelative(marker)
        val px = rx * iw
        val py = ih - ry * ih

        //if (r >= 0 && ox <= model.width && oy >= 0 && oy <= model.height) {
        if (rx >= 0d && rx <= 1d && ry >= 0d && ry <= 1d) {
          val (who, depth) = whoAndDeepness(rx, ry, 1000)
          val at = atMarker(x, y, side, 1D)
          for ((cls, weight, number, offset) <- at) {
            if (who!=side ||true|| cls.weight * cls.weight * weight + weight * depth * depth > 1000*unitDepth) {
              val finalX = (((x + offset._1) - model.widthOffset) / model.width) * iw
              val finalY = ih - ((((y + offset._2) - model.heightOffset) / model.height) * ih)

              //              val finalX = (px+imgOffX).toInt
              //              val finalY = (px-imgOffY).toInt

              val scale = calculateScale(number)
//println("cls:"+cls);if(cls.toString.contains("tiller"))
              drawObject(finalX.toInt, finalY.toInt, scale, side, depth, cls)
            }
          }
        }
      }
    }
    /**
     * a more fault tolerant "inversed" version of forSide ("for(n<-nstatic)for(m<-marker)" instead of "for(m<-marker)for(n<-nstatic)")
     */
    def nstatics{
    	val radius:Double = conf.units.groundRadius.apply.toDouble * 10
    	def distance(ax: Double, ay: Double,mx: Double, my: Double):Double = {
	        val dx = ax - mx
	        val dy = ay - my
	        math.sqrt(dx * dx + dy * dy)
	    }
    	class Counter(mx:Double,my:Double) {
    		def this(xys:(Double,Double,Int))=this(xys._1,xys._2)
    		def dist(ax: Double, ay: Double):Double = distance(ax,ay,mx,my)
        var i = 0d
        var num = 0
        var cumulatedOffX = 0D
        var cumulatedOffY = 0D
        // in game coords
        def offX = cumulatedOffX / num
        // in game coords
        def offY = cumulatedOffY / num
        /**
         * weight within the radius is constant, drops off gently outside 
         */
        def addSoft(ax: Double, ay: Double) = {
          val update = radius / math.max(radius, dist(ax, ay))
          val countUpdate = if (radius > dist(ax, ay)) 1d else 0d
//          val countUpdate = update
          i = i + update
          num += countUpdate.toInt
          cumulatedOffX += countUpdate * (ax - mx)
          cumulatedOffY += countUpdate * (ay - my)
        }
        def addHard(ax: Double, ay: Double) = {
          val update = if (radius > dist(ax, ay)) 1d else 0d
          i = i + update
          num += update.toInt
          cumulatedOffX += update * (ax - mx)
          cumulatedOffY += update * (ay - my)
        }
        def thisOrOther(thisCls: GroundClass.GC, oCls: GroundClass.GC, oCnt: Double): (GroundClass.GC, Double) = {
          if (oCnt > i) (oCls, oCnt) else (thisCls, this.i)
        }

        override def toString = num + ":w(" + i + ")@" + offX + "," + offY
      }
    	val markers = model.front
    	val markerCounters = Map() ++ markers.map(f=>{
    		
    		def maker()={
    			new mutable.HashMap[GroundClass.GC, Counter]()
    		}
    	//	(f,(new Counter(f),new Counter(f) ))}
    		(f,(maker(),maker() ))}
    	)
    	for ((cls, x, y, side) <- model.rawGroundUnits) if(cls.weight > 0) {
    		val withDist: List[((Double, Double, Int), Double)]=markers.map(m=> (m, distance(m._1,m._2,x,y)))

    		if(withDist.isEmpty) {new Counter((0d,0d,0))} else{
    		
	    		val (closest,dist) =  withDist min Ordering[Double].on[(_,Double)](_._2)
	    		
	    		// increase search radius for units on foreign territory (because there's not really a reason to expect them _at_ the enemy front marker...)
	    		val sideRadiusFactor = if(closest._3 != side) 4 else 1
	    		
	    		if(dist<radius * sideRadiusFactor){
	    			
	    			val rb=markerCounters(closest)
	    			val cntMap = if(side==1)rb._1 else rb._2 
	    			val cnt = cntMap.get(cls).getOrElse{
	    				val ncnt=new Counter(closest)
	    				cntMap+=((cls,ncnt))
	    				ncnt
	    			}
	    			cnt.addHard(x, y)
    		  }
    		}
      }
    	// iterate over markerCounters for sides
    	for((marker,bothCounters)<-markerCounters){
    		val(rx,ry) = gameToRelative((marker._1, marker._2))
    		val markerSide = marker._3
    		val (who, depth) = whoAndDeepness(rx, ry, 1000)
    		
    		var sidesList = (bothCounters._1, 1)::(bothCounters._2, 2)::Nil
 
    		// paint foreign groups last
    		if(markerSide==2) sidesList = sidesList.reverse
    		
    		for((counters, side) <- sidesList) if(!counters.isEmpty) {

    			
    			
	        val isAirfield = counters.get(GroundClass.Airfield).map(counter =>
	          counter.num > 0
	          ).getOrElse(false)
	
	        if (isAirfield) for (fuelCount <- counters.get(GroundClass.Fuel)) {
	          val airfieldFuelPenalty = GroundClass.Fuel.weight / 2
	          fuelCount.i = fuelCount.i / airfieldFuelPenalty
	        }
	
	        val (cls, counter) = counters.max(Ordering[Double].on[(GroundClass.GC, Counter)](cc => {
    				val cls = cc._1
    				val cnt = cc._2
    				cnt.i * cls.weight
    			}))
	        
	        //val ret = map.max(order)
//	        val (cls, counter) = map.max(order)
	        log debug ("  identified " + counter + " " + cls + " from " + counters)
	        val (weight, number, offset) = (counter.i, counter.num, (counter.offX.toInt, counter.offY.toInt))
    			

            if (markerSide!=side || cls.weight * cls.weight * weight + weight * depth * depth > 1000*unitDepth) {
              val finalX = (((marker._1 + offset._1) - model.widthOffset) / model.width) * iw
              val finalY = ih - ((((marker._2 + offset._2) - model.heightOffset) / model.height) * ih)

              //              val finalX = (px+imgOffX).toInt
              //              val finalY = (px-imgOffY).toInt

              val scale = calculateScale(number)
//println("cls:"+cls);if(cls.toString.contains("tiller"))
              drawObject(finalX.toInt, finalY.toInt, scale, side, depth, cls)
            }
    		}
    	}
    }
    
    private def groupChiefs = { import model.Chief // interesting instance-import!
    	val groupedMap = new mutable.HashMap[String, ChiefGroup]
    	class ChiefGroup(val name:String,  val chief:Chief, val members:List[String]){
    		private var memberOfBigger=false
    		def use() = memberOfBigger=true
    		def used = memberOfBigger
    		def unused = ! memberOfBigger
    		lazy val memberChiefs = {
//println("members of "+name+" : "+members)    			
    			members.map(groupedMap(_)).toList
    		}
    		val count = chief.count
    		//def size = memberChiefs.count( _ unused)
    		def size = memberChiefs.filter(_ unused).map(_ count).foldLeft[Int](0)(_+_)
    		def isBigger0 = size>0
    	}
    	
    	for ((name, chief) <- model.chiefs) if( ! chief.path.isEmpty){
    		groupedMap.put(name, {
    			val ourStart = chief.path.headOption.get
    			val ourEnd = chief.path.lastOption.get
    			new ChiefGroup(name, chief, model.chiefs.filter{ case(on, oc) => 
	    			if(
	    					oc.path.isEmpty || 
	    					chief.side!=oc.side ||
	    					chief.cls != oc.cls
	    			) false else {
	    				val theirStart = oc.path.headOption.get
	    				val theirEnd = oc.path.lastOption.get
	    				
	    				val xDiffStart = theirStart._1-ourStart._1 
	    				val yDiffStart = theirStart._2-ourStart._2 
	    				val xDiffEnd = theirEnd._1-ourEnd._1 
	    				val yDiffEnd = theirEnd._2-ourEnd._2 
	    				val distanceStart = math.sqrt(xDiffStart*xDiffStart+yDiffStart*yDiffStart)
	    				val distanceEnd = math.sqrt(xDiffEnd*xDiffEnd+yDiffEnd*yDiffEnd)
	    				
	    				(distanceStart+distanceEnd < 5000)
	    			}
    			}.map(_ _1).toList)
    		})
    	}
    	
    	var ret : List[(Chief, Int)] = Nil
    	
    	var nonempties = groupedMap.values.toList
    	while( ! nonempties.isEmpty){
    		val biggest = nonempties.max(Ordering[Int].on{cg:ChiefGroup =>cg.size})
    		ret = (biggest.chief, biggest.size) :: ret
    		for(m<-biggest.memberChiefs) m.use()
    		nonempties = groupedMap.values.filter(_ isBigger0).toList
    	}
    	
    	ret
    }
    
    def forChiefs {
      println("drawing " + model.chiefs.size + " chiefs")
      for ((name, chief) <- model.chiefs) {
        println(name + " side:" + chief.side + " -> " + chief.path)
      }
      
      val doGroupChiefs = true &&
//      	false &&
      	true
       
      val countedChiefs = if(doGroupChiefs) {
    		groupChiefs 
    	} else {
    		model.chiefs.values
    			.filter{chief => chief.side.isDefined && !chief.path.isEmpty}
    			.map{chief=>(chief, chief.count)}
    			.toList
    	}
//      for ((name, chief) <- model.chiefs) if (chief.side.isDefined && !chief.path.isEmpty) {
//      	val count = name match {
//          case MisRender.leadingDigits(nums) => nums.toInt
//          case MisRender.containsColumn() => 5
//          case MisRender.containsTrain() => 10
//        }
//      for((chief, count)<- groupChiefs)	{
      for((chief, count)<- countedChiefs
      		; side <- chief.side
      		)	{
        val start = chief.path.head
        val end = chief.path.lastOption.get
        val cls = chief.cls


        //        val (ex, ey) = end
        val (x, y) = start
        val ox = x - model.widthOffset
        val oy = y - model.heightOffset
        val rx = ox / model.width
        val ry = oy / model.height
        val px = rx * iw
        val py = ih - ry * ih

        
        
        val scale = 0.3D + (math.log(count) * 0.02)

        val (who, depth) = whoAndDeepness(rx, ry, 1000)
        val weight = count
        if (who!=side || cls.weight * cls.weight * weight + weight * depth * depth > 10000*chiefDepth) {

          println("chief " + cls+":"+chief.name+ " at " + px + "," + py + " count:" + count + " scale:" + scale)

          if(debugMode) for(xml<-mapWriter) xml += <area shape="circle" coords={px.toInt+","+py.toInt+",10"} title={chief.name} href="#" ></area>
          
          var lastX = px.toInt
          var lastY = py.toInt
          

          for ((ix, iy) <- chief.path.tail) {
            val finalX = ((((ix) - model.widthOffset) / model.width) * iw).toInt
            val finalY = ih - (((((iy) - model.heightOffset) / model.height) * ih)).toInt

            if (debugMode) { // draw exact path
              ig2.setColor(awt.Color.green)
              ig2.drawLine(lastX, lastY, finalX, finalY)
            }
            lastX = finalX
            lastY = finalY
          }

          if (debugMode) { // full vector
            ig2.setColor(awt.Color.white)
            ig2.drawLine(px.toInt, py.toInt, lastX, lastY)
          }
          var skipFirst = 15
          var useMax = 5
//          skipFirst = 0
//          useMax = 0
          
          val sumsCount = chief.path.take(skipFirst).takeRight(useMax).foldLeft(0D, 0D, 0) { (acc, us) =>
            (acc._1 + us._1, acc._2 + us._2, acc._3 + 1)
          }
          val (avgX, avgY) = {
            val r1 = (sumsCount._1 / sumsCount._3,
              sumsCount._2 / sumsCount._3)

            (((((r1._1) - model.widthOffset) / model.width) * iw),
              ih - (((((r1._2) - model.heightOffset) / model.height) * ih)))
          }

          val difX = avgX - px
          val difY = avgY - py
          val len = math.sqrt((difX * difX) + (difY * difY)
            )
          if(debugMode){
          	ig2.setColor(awt.Color.BLACK)      
          	ig2.drawOval(avgX.toInt-4, avgY.toInt-4,8,8)
          }
          println("arrow at "+avgX+","+avgY+" len :"+len) 
          if(len>conf.chiefs.moveThreshold.apply){
          	drawObject(px.toInt, py.toInt, scale * 4D, side, 1000, GroundClass.ChiefMove, Some((difX, difY)))
          }else{
          	val dir = if(difX!=0) difX else if(difY!=0) difY else (side.toDouble - 1.5)
          	drawObject(px.toInt, py.toInt, scale * 2.5D, side, 1000, GroundClass.ChiefStand, Some(dir, -math.abs(dir)))
          }
          drawObject(px.toInt, py.toInt, scale, side, 255, cls)
          if(debugMode){
//          	ig2.
          	val font = ig2.getFont
          	ig2.setColor(awt.Color.black)
          	ig2.setFont(new java.awt.Font(font.getName, font.getStyle, 12))
          	ig2.drawString(""+chief.name, px.toInt+10, py.toInt+5)
          }
                    
        }
      }
    }
//    forChiefs
//    forSide(1)
//    forSide(2)
//  }

  lazy val drawInit = {
    import java.awt._
    ig2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val font = new Font("SansSerif", Font.PLAIN, 8);
    ig2.setFont(font);
  }

  def drawObject(x: Int, y: Int, scale: Double, side: Int, depth: Int, cls: GroundClass.GC, rotation: Option[(Double, Double)] = None) {

    drawInit
    ig2.freeze {
      val colDepth = math.max(0, math.min(depth / 2, 255))
      //try{
      // set criteria to > 0 for disabling long range visibility of fuel etc
      if (colDepth >= 0) {
        //      ig2.setColor(Color.black)
        val (randx, randy) = randomizeLocations(x, y, 1D / colDepth.toDouble)

        for (img <- spritesMaker.paintable(cls, side)) {
          val (width, height) = img.dimensions
          val factor = 1D

          val t = new AffineTransform()
          t.setToIdentity

          t.translate(randx, randy)

          t.scale(factor * scale, factor * scale)
          ig2.setColor(awt.Color.green)
          ig2.drawLine(randx, randy, x, y)

          try ig2 freeze {
            //  var rot = t
            //          rot = AffineTransform.getRotateInstance(100, 1, x, y)
            //          rot.concatenate(t)
            //        
            img.paint(t, colDepth.toDouble / 255D, ig2, rotation)
          } catch { case x => println("rot failed " + x.getClass.getCanonicalName + " : " + x.getMessage) }
        }

      }
      //}catch{case x => log.error("failed rotation ",x) }
    }
  }
  def randomizeLocations(x: Int, y: Int, depth: Double): (Int, Int) = {
    import java.awt.geom._

    if (!randomize) (x, y) else {
      val direction = math.Pi * ((x, y).hashCode % 720).toDouble / 360
      val distance = math.min(10D, 200D * math.sqrt(depth))

      val ret = (x + (math.cos(direction) * distance).toInt,
        y + (math.sin(direction) * distance).toInt)
      ret
    }

  }

  def front(steps: Int, aa: Int) {
    val dimension = ih + iw
    val scale = 1 << aa

    // set true to allow uninterpolated values for front calculation  
    var precise = false
    //        precise = true    
    ig2.scale(1.toDouble / scale.toDouble, 1.toDouble / scale.toDouble)

    val sih = ih * scale
    val siw = iw * scale
    // has to be 2^x !
    val initialStep = scale << (steps);

    def dif(x: Int, y: Int): Double = {
      //      val px: Double = (x.toDouble - model.widthOffset) / siw.toDouble
      //      val py: Double = 1 - (y.toDouble - model.heightOffset) / sih.toDouble
      val px: Double = (x.toDouble) / siw.toDouble
      val py: Double = 1 - (y.toDouble) / sih.toDouble

      val (r, b) = if (precise) (preciseSideVal(px, py, model.rfront),
        preciseSideVal(px, py, model.bfront))
      else (sideVal(px, py, model.rfront),
        sideVal(px, py, model.bfront))
      r - b
    }

    def differentSides(vals: Double*): Boolean = {
      def signum(d: Double): Double = {
        if (d.isNaN || Double.NaN == d) 0
        else math.signum(d)
      }
      val sig = signum(vals.head)
      vals.exists(x => signum(x) != sig)
    }
    def pixel(x: Int, y: Int, me: Double, other: Double) {

      val max = 200
      val min = 50
      val deep = (math.min(max, min + (max - min).toDouble * math.abs(me / dimension)) / scale).toInt
      if (me < 0) ig2.setColor(new java.awt.Color(255, 100, 0, deep))
      else if (me > 0) ig2.setColor(new java.awt.Color(0, 100, 255, deep))

      ig2.fillOval(x - scale, y - scale, scale * 2, scale * 2)
    }
    /**
     *   ul  u  ur
     *  
     *   l   c   r
     * 
     *   bl  b  br
     */
    def tile(x: Int, y: Int, step: Int, ul: Double, ur: Double, bl: Double, br: Double) {

      if (step == 1) {
        //println("                                                   stepping "+step+" level "+x+","+y)           
        if (differentSides(ul, ur)) {
          pixel(x, y, ul, ur)
          pixel(x + 1, y, ur, ul)
        }
        if (differentSides(ul, bl)) {
          pixel(x, y, ul, bl)
          pixel(x, y + 1, bl, ul)
        }
        //          if(x%2==0 && y%2==0){
        if (x % 2 == 0) {
          // recurse half steps
          val rur = dif(x + 2, y)
          val rbr = dif(x + 2, y + 1)
          tile(x + 1, y, 1, ur, rur, br, rbr)
        }
        if (y % 2 == 0) {
          val bbl = dif(x + 2, y)
          val bbr = dif(x + 2, y + 1)
          tile(x, y + 1, 1, bl, br, bbl, bbr)
        }
      } else {
        val half = step / 2
        val l = dif(x, y + half)
        val c = dif(x + half, y + half)
        val r = dif(x + step, y + half)
        val u = dif(x + half, y)
        val b = dif(x + half, y + step)

        if (differentSides(ul, u, l, c)) {
          tile(x, y, half, ul, u, l, c)
        }
        if (differentSides(u, ur, c, r)) {
          tile(x + half, y, half, u, ur, c, r)
        }
        if (differentSides(l, c, bl, b)) {
          tile(x, y + half, half, l, c, bl, b)
        }
        if (differentSides(c, r, b, br)) {
          tile(x + half, y + half, half, c, r, b, br)
        }

        if (step == initialStep) {
          //println("stepped "+step+" level "+x+","+y)           
          if (x + 2 * step < siw) { // setp right
            val rur = dif(x + 2 * step, y)
            val rbr = dif(x + 2 * step, y + step)
            tile(x + step, y, step, ur, rur, br, rbr)
          }
          if (x == 0 && y + 2 * step < sih) { // step down
            val bbl = dif(x, y + 2 * step)
            val bbr = dif(x + step, y + 2 * step)
            tile(x, y + step, step, bl, br, bbl, bbr)
          }
        }
      }
    }

    tile(0, 0, initialStep, dif(0, 0), dif(0, initialStep), dif(initialStep, 0), dif(initialStep, initialStep))

    ig2.scale(scale, scale)
  }

  /**
   * the core hatching function
   * 
   * @param px 0..1
   * @param py 0..1
   * @param max 0..255?
   * @return (side number, "deepness")
   */
  def whoAndDeepness(px: Double, py: Double, max: Int): (Int, Int) = {
    val r = sideVal(px, py, model.rfront)
    val b = sideVal(px, py, model.bfront)

    val (us: Double, them: Double, who: Int) =
      if (r > b) (r, b, 1)
      else if (b > r) (b, r, 2)
      else (0.toDouble, 0.toDouble, 0)

    val dimension = model.height + model.width
    val threshold = 10000 / (dimension)
    //if(us-them > threshold && who>0){
    val advantage = us - them
    if (advantage >= 0 && who >= 0) {
      //println("drawing "+who+" at "+x+","+y)
      val min = 0

      val offset = 0
      //var deep = min+((max-min)*math.pow((them+offset)/(offset+us-them), 1.)).toInt
      var deep = max - ((max - min) * math.pow((us - them) / (us + us * them), 1)).toInt
      //            var deep = max-((max-min)*math.pow(((us-them)/(us+us*them * ((us-(them+threshold))))), 4)).toInt    

      deep = math.min(max, deep)

      if (advantage < threshold) {
        val out = advantage / threshold
        deep = deep - (deep * (1 - out)).toInt
        deep = math.min(max, deep)
      }

      //deep = deep + (255*math.min(0, us-(them+threshold)/(us-them))).toInt

      //            var deep = max-((max-min)*math.pow((us+10)/(10+us-them), 0.1)).toInt                                                             
      deep = math.max(0, deep)
      (who, deep)
    } else (0, 0)
  }

  def hatch(step:Int) {
log.debug("hatching...")
var drawn = "nothing drawn"
    for {
      x <- 1 to iw - 1
      y <- 1 to ih - 1
    } {
      val isR = ((x + y) % step == 0)
      val isB = ((x - y) % step == 0)
      val max = 70
      def draw(who: Int, deep: Int) {
        ig2.setColor(new java.awt.Color(0, 0, 0, 0))
        if (who == 1 && isR) ig2.setColor(new java.awt.Color(255, 0, 0, deep))
        else if (who == 2 && isB) ig2.setColor(new java.awt.Color(0, 0, 255, deep))
        else if (who == 0) {
          if (isR && isB) ig2.setColor(new java.awt.Color(0, 0, 0, max))
          else if (isR) ig2.setColor(new java.awt.Color(255, 0, 0, max))
          else if (isB) ig2.setColor(new java.awt.Color(0, 0, 255, max))
        }
        
        ig2.drawLine(x, y, x+1, y)
var drawn = "drawn ("+x+","+y+")"        
      }

      if (isR || isB) {
        val px: Double = x.toDouble / iw.toDouble
        val py: Double = 1 - y.toDouble / ih.toDouble

        val dp = whoAndDeepness(px, py, max)
        draw(dp._1, dp._2)

      }
      //          else{
      //            ig2.setColor(new java.awt.Color(0, 255, 0, 200))
      //            ig2.drawLine(x,y,x,y)
      //          }

      ()
    }
log.debug("...hatching done "+drawn)

    ()
  }
}