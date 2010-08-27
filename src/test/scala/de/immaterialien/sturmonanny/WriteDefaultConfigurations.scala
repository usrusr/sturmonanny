package de.immaterialien.sturmonanny
import org.junit.Test
import _root_.de.immaterialien.sturmonanny.util
import _root_.de.immaterialien.sturmonanny.core
import _root_.de.immaterialien.sturmonanny.global

object WriteDefaultConfigurations { 
  def main(args : Array[String]) : Unit = {
println("default...")    
		write("default", new core.Configuration(null, null))
//    new java.io.FileWriter("zip/default.conf").write(
//				new core.Configuration(null).toString
//	  )
	 
println("global...")
		write("global", new global.GlobalConfig(null))
		
//	  new java.io.FileWriter("zip/global.conf").write(
//			  new global.GlobalConfig(null).toString		
//	  )
	  
println("instances...")    
		write("instances", new global.Instances(null, null)) 
//	  new java.io.FileWriter("zip/instances.conf").write(
//				new global.Instances(null).toString
//	  )
  }
  def write(name:String, conf:util.configgy.ConfigurationSchema){
	  val pref = "empty-configs/"
	  val suff = ".conf"
	  val fname=pref+name+suff
	  val cont = conf.toString
println("\n\nfile "+fname+"\n"+cont)
	  val out = new java.io.FileWriter(fname)
	  out write cont
    out.close
  }
}

class WriteDefaultConfigurations {
   @Test 
   def run() : Unit ={
     WriteDefaultConfigurations.main(null)
   }
}