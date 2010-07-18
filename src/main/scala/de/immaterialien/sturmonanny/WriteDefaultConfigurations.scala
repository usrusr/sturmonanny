package de.immaterialien.sturmonanny
 

object WriteDefaultConfigurations {  
  def main(args : Array[String]) : Unit = {
println("default...")    
		write("default", new core.Configuration(null, null))
	 
println("global...")
		write("global", new global.GlobalConfig(null))
	  
println("instances...")    
		write("instances", new global.Instances(null, null))
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
