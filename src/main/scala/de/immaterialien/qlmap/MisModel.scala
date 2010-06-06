package de.immaterialien.qlmap

import scala.xml._

import scala.swing
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

class MisModel {
	var front : List[(Double, Double, Int)] = Nil
	var rfront : List[(Double, Double)] = Nil
	var bfront : List[(Double, Double)] = Nil
	var width = 160000.
	var height = 160000.
	def frontMarker(x:Double, y:Double, a:Int) {
	  front = (x,y,a)::front
	  if(a==1) rfront = (x,y)::rfront
	  if(a==2) bfront = (x,y)::bfront
	}
 	def bornPlace(a:Integer, x:Double, y:Double) {
 	  
 	}

   def sideVal (x:Double, y:Double, markers:List[(Double, Double)]) : Double = {
     val zero:(Double, Double)=(0,0)
     val ret = markers.foldLeft(zero)(  ((acc:(Double, Double), coord:(Double, Double))=>{
       val (px, py) = coord
       val xd = (x*width-px)
       val yd = (y*height-py)
       val dist = Math.sqrt(xd*xd + yd*yd)
       val dimension = width+height
       val add = (Math.pow(dimension/dist, 3))/dist       
//       val add = dimension*dimension*dimension/(dist*dist*dist*dist)
//       val add = dimension*dimension/ (dist*dist*dist) 
       (acc._1 + add, Math.max(acc._2, add)) 
      })) 
                                 
   	(ret._1/markers.size)*1 + ret._2                                     
   }
  
   def paint(in:BufferedImage):BufferedImage={
     paint(in, 4, 3)
     hatch(in)
   }

   def paint(in:BufferedImage, steps:Int, aa:Int):BufferedImage={
     var ih = in.getHeight 
     var iw = in.getWidth
     
     val dimension = ih+iw
     val ig2 = in.createGraphics();
     val scale=1<<aa
     
     //ig2.transform(java.awt.geom.AffineTransform.getScaleInstance(1.1,1.1))
      //ig2.transform(java.awt.geom.AffineTransform.getScaleInstance(1.toDouble/scale.toDouble,1.toDouble/scale.toDouble))
     ig2.scale(1.toDouble/scale.toDouble,1.toDouble/scale.toDouble)

     ih=ih*scale
     iw=iw*scale
     // has to be 2^x !
     val initialStep = scale<<(steps);

     def dif(x:Int, y:Int) : Double = {
		   val px : Double = x.toDouble/iw.toDouble 
		   val py : Double = 1 - y.toDouble/ih.toDouble
   	  val r = sideVal(px, py, rfront)
   	  val b = sideVal(px, py, bfront)
   	  r-b
     }
     
     def differentSides(vals : Double*) : Boolean = {
       val sig = Math.signum(vals.first)
       vals.exists(x => Math.signum(x) != sig)
//       var sig = Math.signum(vals.first)
//       vals.exists(x => {
//         val nSig = Math.signum(x)
//         if(sig==0) sig=nSig
//         nSig != sig
//       })
     }
     def pixel(x:Int, y:Int, me:Double, other:Double){
       
       val max=200
       val min=50
       val deep=(Math.min(max, min+(max-min).toDouble * Math.abs(me/dimension))/scale).toInt
//println("                                                                         pixel "+x+","+y+ "  "+deep+"   "+me+" vs "+other)           
//println("                                                                                      me is   "+dif(x,y))          
       
//       if(me>=0) ig2.setColor(new java.awt.Color(255,0,0, deep)) 
//		 else if(me<=0 * -1) ig2.setColor(new java.awt.Color(0, 0, 255, deep))
       if(me<0) ig2.setColor(new java.awt.Color(255,100,0, deep)) 
		 else if(me>0) ig2.setColor(new java.awt.Color(0, 100, 255, deep))
		 //ig2.drawLine(x,y,x,y)
   
		
   
       ig2.fillOval(x-scale,y-scale,scale*2,scale*2) 
     }
     /**
      *   ul  u  ur
      *  
      *   l   c   r
      * 
      *   bl  b  br
      */
     def tile(x:Int, y:Int, step:Int, ul:Double, ur:Double, bl:Double, br:Double){
       
       if(step==1){
//println("                                                   stepping "+step+" level "+x+","+y)           
          if(differentSides(ul, ur)){
            pixel(x,y, ul, ur)
            pixel(x+1,y, ur, ul)
          }
          if(differentSides(ul, bl)){
            pixel(x,y, ul, bl)
            pixel(x,y+1, bl, ul)
          }
//          if(x%2==0 && y%2==0){
          if(x%2==0){
            // recurse half steps
            val rur=dif(x+2, y)
	   		val rbr=dif(x+2, y+1)
	   		tile(x+1,y,1,ur,rur,br,rbr)
	   	 }
	   	 if(y%2==0){
            val bbl=dif(x+2, y)
	   		val bbr=dif(x+2, y+1)
	   		tile(x,y+1,1,bl,br,bbl,bbr)
          }
       }else{
      	 val half = step/2
      	 val l = dif(x, y+half)
      	 val c = dif(x+half, y+half)
      	 val r = dif(x+step, y+half)
      	 val u = dif(x+half, y)
      	 val b = dif(x+half, y+step)

//println("                  stepping "+step+" level "+x+","+y +" center value: "+c)           
        
//tile(x,y,half, ul,u,l,c)
//tile(x+half,y,half, u, ur, c, r)
//tile(x,y+half,half, l, c, bl, b)
//tile(x+half,y+half,half, c, r, b, br)
        
      	 if(differentSides(ul, u, l, c)){
      	   tile(x,y,half, ul,u,l,c)
      	 }
      	 if(differentSides( u, ur, c, r)){
      	   tile(x+half,y,half, u, ur, c, r)
      	 }
      	 if(differentSides(l, c, bl, b)){
      	   tile(x,y+half,half, l, c, bl, b)
      	 }
      	 if(differentSides(c, r, b, br)){
      	   tile(x+half,y+half,half, c, r, b, br)
      	 }
       
       
      	 if(step==initialStep){
//println("stepped "+step+" level "+x+","+y)      	   
	   		if(x+2*step<iw){ // setp right
	   		  val rur=dif(x+2*step, y)
	   		  val rbr=dif(x+2*step, y+step)
	   		  tile(x+step, y, step, ur, rur, br, rbr)
	   		}
	   		if(x==0 && y+2*step<ih){ // step down
	   		  val bbl=dif(x, y+2*step)
	   		  val bbr=dif(x+step, y+2*step)
	   		  tile(x, y+step, step, bl, br, bbl, bbr)
	   		}
	   	 }
       }
     }
     
     
     tile(0,0,initialStep, dif(0,0), dif(0,initialStep),dif(initialStep,0), dif(initialStep,initialStep))
     
     //ig2.scale(1/scale, 1/scale)
    
     ig2.dispose
     in
   }
   
   
   
   
   def hatch(in:BufferedImage):BufferedImage={
     val step = 10;
     
     val ig2 = in.createGraphics();
     
     val ih = in.getHeight 
     val iw = in.getWidth 
     
     for{x <- 1 to iw-1
     		y <- 1 to ih-1}{
     		  val isR = ((x+y)%step==0)
     		  val isB = ((x-y)%step==0)
     		  
     		  if(isR||isB){
     			   val px : Double = x.toDouble/iw.toDouble 
     			   val py : Double = 1 - y.toDouble/ih.toDouble
           
     			  	val r = sideVal(px, py, rfront)
 					val b = sideVal(px, py, bfront)
      
 					val (us:Double, them:Double, who:Int) = 
 						if(r>b) (r, b, 1) 
                  else if(b>r) (b, r, 2) 
                  else (0.toDouble,0.toDouble,0)
                  
               val dimension = height+width
               val threshold = 10000 / ( dimension)
 					//if(us-them > threshold && who>0){
               val advantage = us-them
 					if(advantage >= 0 && who>=0){
//println("drawing "+who+" at "+x+","+y)
 					  	val min = 0
 					  	val max = 70
 					  	val offset = 0
 					  	//var deep = min+((max-min)*Math.pow((them+offset)/(offset+us-them), 1.)).toInt
 					  	var deep = max-((max-min)*Math.pow((us-them)/(us+us*them), 1)).toInt
//						var deep = max-((max-min)*Math.pow(((us-them)/(us+us*them * ((us-(them+threshold))))), 4)).toInt    

  					  	deep = Math.min(max, deep)

  					  	if(advantage < threshold){
  					  	  val out = advantage/threshold
  					  	  deep = deep - (deep*(1-out)).toInt
  					  	  deep = Math.min(max, deep)
         
  					  	}

						//deep = deep + (255*Math.min(0, us-(them+threshold)/(us-them))).toInt
      
//						var deep = max-((max-min)*Math.pow((us+10)/(10+us-them), 0.1)).toInt                                                             
 					  	deep = Math.max(0, deep)
 					  	ig2.setColor(new java.awt.Color(0,0,0,0))
						if(who==1 && isR) ig2.setColor(new java.awt.Color(255,0,0, deep))
						else if(who==2 && isB) ig2.setColor(new java.awt.Color(0, 0, 255, deep))
						else if(who==0){
						  if(isR && isB) ig2.setColor(new java.awt.Color(0, 0, 0, max))
						  else if(isR) ig2.setColor(new java.awt.Color(255, 0, 0, max))
						  else if(isB) ig2.setColor(new java.awt.Color(0, 0, 255, max))
						}
 					   ig2.drawLine(x,y,x,y)
         
 					}
// 					else{
// 					  ig2.setColor(new java.awt.Color(0, 255, 0, 200))
// 					  ig2.drawLine(x,y,x,y)
// 					}
      
 					()
     		  }
         
     		  ()
     		}
    
     ig2.dispose
     in
   }
  
 	def toHtml():NodeSeq = {
 	  var out = 
 		   <div style="position:inherit; float:left; border: thick; border-color: red; border-style: solid" >
 	  			<div style="position: relative; width: 100%; display: block;" > 
 	  				<img src="src/test/resources/Italy_Online.jpg" style="size: 100%; display: block;"/>
 	  				{front map frontMarkers}
 	  				{htmlFront}
    
 	  			</div>
 	  		</div>
 	  for(f<-front){
 	    
 	  }
 	  out
 	}
  
  def htmlPos(x:Double, y:Double):Node = {
 	val r = sideVal(x, y, rfront) 
 	val b = sideVal(x, y, bfront) 
   val color = {if(r>b) "red" else "blue"}
   val singleMagnitude = 100*{if(r>b) b else r}/(width*height)
   val magnitude = 1*Math.sqrt(singleMagnitude)
 		(<div position="relative"
 		  		style={
 		  				"display: block; float: left; position:absolute; top: "+(100*(1-y)).toInt+"%; left:" +(100*x).toInt+"%"         
 		  		}
        ><div style={"position:relative;top:-1ex;left:-1ex;color:"+color+";"}>{magnitude.toInt}</div></div>)
   }
  
   def htmlFront : NodeSeq = { 
     val count = 75
     for{x <- 0 to count
         y <- 0 to count} yield htmlPos(x.toDouble / count, y.toDouble / count)
    }
  
  var i=0;
 	def frontMarkers(pair :(Double, Double,Int)): NodeSeq = {
 		  <div position="relative"
 		  		style={
 		  				"display: block; float: left; position:absolute; top: "+(100-100*pair._2/width).toInt+"%; left:" +(100*pair._1/height).toInt+"%"         
 		  		}
        >{pair._3}:{ i=i+1; i }</div>
 	}
}
