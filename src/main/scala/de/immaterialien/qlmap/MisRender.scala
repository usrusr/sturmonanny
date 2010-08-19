package de.immaterialien.qlmap

import javax.imageio.ImageIO
import java.awt.image._
import java.awt.geom._
import java.io
import scala.collection._

object MisRender extends Log{
  def paint(forMission: io.File, model: MisModel, outputPath : Option[io.File] = None): Option[io.File] = try{

    var format: String = null

//     reanimate to enable PNG output...    
//         val iis = ImageIO.createImageInputStream(model.imageFile)
//         val readers = ImageIO.getImageReaders(iis)
//         while(format==null && readers.hasNext) {
//           val reader = readers.next
//           format = reader.getFormatName 
//         }
//         iis.close
         
         // jpg
    if (format == null) format = "JPG"
//format = "PNG"      
    var instream:java.io.InputStream = null
    var ig:java.awt.Graphics2D = null
    try {
      instream = model.imageFile
      var in = ImageIO.read(instream)
      ig = in.createGraphics()
      new MisRender(
        model,
        ig,
        in.getHeight,
        in.getWidth
        ).sequence(in)
      val outPath = outputPath.getOrElse(forMission.getParentFile)
      val outFile = new java.io.File(outPath, forMission.getName + "." + format)
      ImageIO.write(in, format, outFile)
      Some(outFile)
    }finally{
      try instream.close
      try ig.dispose
    }
  }catch{case _ =>None}
}

private class MisRender (
  model: MisModel,
  ig2: java.awt.Graphics2D,
  ih: Int,
  iw: Int) extends Log{

  val randomize = true && false
  val interpolate = 5
    var xsvr = iw/interpolate
    var ysvr = ih/interpolate
    lazy val svrs ={
      val xsvr = iw 
      Array.fromFunction{(_,_,_)=>
        Double.NaN
      }(2,xsvr+2,ysvr+2)
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
  def sideVal(x: Double, y: Double, markers: List[(Double, Double)]): Double = {
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

    val ix = Math.floor(x * xsvr).toInt
    val iy = Math.floor(y * ysvr).toInt
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
      val dist = Math.sqrt(xd * xd + yd * yd)
      val dimension = model.width + model.height
      val add = (Math.pow(dimension / dist, 3)) / dist
      //       val add = dimension*dimension*dimension/(dist*dist*dist*dist)
      //       val add = dimension*dimension/ (dist*dist*dist) 
      (acc._1 + add, Math.max(acc._2, add))
    }))

    (ret._1 / markers.size) * 1 + ret._2
  }

  def sequence(in: BufferedImage) {
    veil()
//        veil()
    front(4, 2)
    hatch() 
    units()
    airfields()
  }
  def veil() {

    val darkness = 30
    val light = 255 - darkness
    val background = 120 // higher -> more base image

    ig2.setColor(new java.awt.Color(light, light, light, 255 - background))
    ig2.fillRect(0, 0, iw, ih)
  }
  def airfields() {
    for ((gx, gy, side) <- model.rawAirFields) {
      val ox = gx - model.widthOffset
      val oy = gy - model.heightOffset
      val rx = ox / model.width
      val ry = oy / model.height
      val px = rx * iw
      val py = ih - ry * ih

      drawObject(px.toInt, py.toInt, 1000, side, 1, GroundClass.Airfield)
    }
  }
  def units() {
    /**
     * returns class, weight, count and relative position of dominant class
     */
    def atMarker(mx: Double, my: Double, mside: Int): Option[(GroundClass.GC, Double, Int, (Int, Int))] = {
      val radius = 5000d
      def dist(ax: Double, ay: Double) = {
        val dx = ax - mx
        val dy = ay - my
        Math.sqrt(dx * dx + dy * dy)
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
          val update = radius / Math.min(radius, dist(ax, ay))
          val countUpdate = if (radius > dist(ax, ay)) 1d else 0d
          i = i + update
          num += countUpdate.toInt
          cumulatedOffX += countUpdate*(ax-mx) 
          cumulatedOffY += countUpdate*(ay-my) 
        }
        def addHard(ax: Double, ay: Double) = {
          val update = if (radius > dist(ax, ay)) 1d else 0d
          i = i + update
          num += update.toInt
          cumulatedOffX += update*(ax-mx) 
          cumulatedOffY += update*(ay-my) 
        }
        def thisOrOther(thisCls: GroundClass.GC, oCls: GroundClass.GC, oCnt: Double): (GroundClass.GC, Double) = {
          if (oCnt > i) (oCls, oCnt) else (thisCls, this.i)
        }

        override def toString = num +":w("+i+")@"+offX+","+offY
      }

      var map: mutable.Map[GroundClass.GC, Counter] = mutable.HashMap()
      for ((cls, x, y, side) <- model.rawGroundUnits; if side == mside) {
        val count = map.get(cls).getOrElse {
          val c = new Counter
          map.put(cls, c)
          c
        }
        count.addHard(x, y)
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

        if (isAirfield) for(fuelCount<-map.get(GroundClass.Fuel)){
          val airfieldFuelPenalty = GroundClass.Fuel.weight / 2
          fuelCount.i = fuelCount.i/airfieldFuelPenalty
        }

        //val ret = map.max(order)
        val (cls, counter) = map.max(order)
        log debug ("  identified "+counter+ " " + cls + " from " + map )
        Some(cls, counter.i, counter.num, (counter.offX.toInt,counter.offY.toInt))

      }
    }
    def forSide(side: Int) {
      val markers = if (side == 1) model.rfront else model.bfront
      val other = if (side == 1) model.bfront else model.rfront
      val sd = if (side == 1) "red" else if (side == 2) "blue" else "???"

      for (marker <- markers) {
        val (x, y) = (marker._1, marker._2)
        val ox = x - model.widthOffset
        val oy = y - model.heightOffset
        val rx = ox / model.width
        val ry = oy / model.height
        val px = rx * iw
        val py = ih - ry * ih

        if (ox >= 0 && ox <= model.width && oy >= 0 && oy <= model.height) {
          val at = atMarker(x, y, side)
          for ((cls, weight, number, offset) <- at) {
            val (who, depth) = whoAndDeepness(rx, ry, 1000)
            if (cls.weight * cls.weight * weight + weight * depth * depth > 100000) {

              val finalX = (((x + offset._1) - model.widthOffset) / model.width) * iw
              val finalY = ih - ((((y + offset._2) - model.heightOffset) / model.height) * ih)
              
//              val finalX = (px+imgOffX).toInt
//              val finalY = (px-imgOffY).toInt
              drawObject(finalX.toInt, finalY.toInt, number, side, depth, cls)
            }
          }
        }
      }
    }
    forSide(1)
    forSide(2)
  }

  lazy val drawInit = {
    import java.awt._
    ig2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val font = new Font("SansSerif", Font.PLAIN, 8);
    ig2.setFont(font);
  }

  def drawObject(x: Int, y: Int, count: Int, side: Int, depth: Int, cls: GroundClass.GC) {
    import java.awt._
    drawInit

    val colDepth = Math.max(0, Math.min(depth / 2, 255))

    // set criteria to > 0 for disabling long range visibility of fuel etc
    if (colDepth >= 0) {
      //      ig2.setColor(Color.black)
      val (randx, randy) = randomizeLocations(x, y, 1D / colDepth.toDouble)

      val scale = {
        var s = 0.3
        var d = count.toDouble
        while (d / 2 > 2) {
          s += 0.08
          d = d / 2
        }
        s
      }

      //ig2.drawString(count + " " + cls+"s ("+(randx -x)+", "+(randy-y) +")" , randx, randy)
      //      val image = sprites.Sprites.forClass(cls, side)
      //      for(img<- image ){
      //        val h2 = (scale * img.getHeight / 2).toInt
      //        val w2 = (scale * img.getWidth / 2).toInt
      for (img <- sprites.Sprites.paintable(cls, side)) {
        val (width, height) = img.dimensions
        val factor = 1D
        //        val factor :Double= cls match {
        //          case GroundClass.Airfield => 2
        //          case GroundClass.Plane => 1
        //          case _ => 1
        //        }

        val h2 = (factor * scale * height / 2).toInt
        val w2 = (factor * scale * width / 2).toInt
        //        new BufferedImageOp()
        //val op = new RescaleOp(scale.toFloat, 0f , null)
        //ig2.drawImage(img, op, randx-w2, randy-h2)
        val t = new AffineTransform()
        t.setToIdentity
        t.translate(randx - w2, randy - h2)
        t.scale(factor * scale, factor * scale)
        img.paint(t, colDepth.toDouble / 255D, ig2)
        //        ig2.drawImage(img, t, null)
        //        val vpadding = 16
        //        val hpadding = 3
        //        ig2.setColor(Color.black)
        //        ig2.drawString(count +"" , hpadding+randx-w2, vpadding+randy-h2)
      }

      ig2.setColor(Color.green)
      ig2.drawLine(randx, randy, x, y)
    }
  }
  def randomizeLocations(x: Int, y: Int, depth: Double): (Int, Int) = {
    import java.awt.geom._

    if (!randomize) (x, y) else {
      val direction = Math.Pi * ((x, y).hashCode % 720).toDouble / 360
      val distance = Math.min(10D, 200D * Math.sqrt(depth))

      val ret = (x + (Math.cos(direction) * distance).toInt,
        y + (Math.sin(direction) * distance).toInt)
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
      val py: Double = 1 - (y.toDouble ) / sih.toDouble

      val (r, b) = if (precise) (preciseSideVal(px, py, model.rfront),
        preciseSideVal(px, py, model.bfront))
      else (sideVal(px, py, model.rfront),
        sideVal(px, py, model.bfront))
      r - b
    }

    def differentSides(vals: Double*): Boolean = {
      def signum(d: Double): Double = {
        if (d.isNaN || Double.NaN == d) 0
        else Math.signum(d)
      }
      val sig = signum(vals.first)
      vals.exists(x => signum(x) != sig)
    }
    def pixel(x: Int, y: Int, me: Double, other: Double) {

      val max = 200
      val min = 50
      val deep = (Math.min(max, min + (max - min).toDouble * Math.abs(me / dimension)) / scale).toInt
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
      //var deep = min+((max-min)*Math.pow((them+offset)/(offset+us-them), 1.)).toInt
      var deep = max - ((max - min) * Math.pow((us - them) / (us + us * them), 1)).toInt
      //            var deep = max-((max-min)*Math.pow(((us-them)/(us+us*them * ((us-(them+threshold))))), 4)).toInt    

      deep = Math.min(max, deep)

      if (advantage < threshold) {
        val out = advantage / threshold
        deep = deep - (deep * (1 - out)).toInt
        deep = Math.min(max, deep)
      }

      //deep = deep + (255*Math.min(0, us-(them+threshold)/(us-them))).toInt

      //            var deep = max-((max-min)*Math.pow((us+10)/(10+us-them), 0.1)).toInt                                                             
      deep = Math.max(0, deep)
      (who, deep)
    } else (0, 0)
  }

  def hatch() {
    val step = 15;

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
        ig2.drawLine(x, y, x, y)
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

    ()
  }
}