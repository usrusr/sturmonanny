package de.immaterialien.qlmap

import java.io._

class MissionFilter(args: String) extends javax.xml.ws.Provider[File] with Log {
  val mapBase = new MapBase(new File(args))
  val groundClasses = new GroundClasses(args)
  override def invoke(file: File): File = {
    
    new Thread("recon "+file.getName){
      override def run {
        val parsed = new MisParser(file, mapBase, groundClasses)
        val img = MisRender.paint(file, parsed.out)
      }
    }.start
  
    file
  }
}