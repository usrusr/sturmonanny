package de.immaterialien.qlmap

import java.io._


class Invoke { import Invoke._

}
object Invoke {
	val dotMis = new FileFilter{
		val re = """(?i)^(.*)\.mis$""".r
		override def accept(f:File) = f.getName match {
			case re(_) => true
			case _ => false
		}
	}
	class WrongArguments(msg:String="see correct usage below") extends Exception(msg)

	def main(args : Array[String]){
		try{
			if(args.isEmpty) throw new WrongArguments
			val confPath = new File(args.head)
			
			if( ! confPath.exists) throw new WrongArguments(confPath.getAbsolutePath+" does not exist")
			
			try {
				val filter = new HtmlMissionFilter(confPath.getAbsolutePath)
			
				for(fname <- args.drop(1)){
					val f = new File(fname)
					if(! f.exists) throw new WrongArguments(f.getAbsolutePath+ " does not exist")
					
					val ls = if(f.isDirectory){
						val l = f.listFiles(dotMis)
						if(l==null) Nil else l.toList
					}else f::Nil
					
					for(mis<-ls) {
						println("rendering "+mis.getAbsolutePath+ "...")
						filter.inline(mis)
					}
				}
			}catch{
				case x => throw new WrongArguments(x.getClass + "("+x.getStackTraceString+")") 
			}

		} catch {
			case wa:WrongArguments=> {
				println("Wrong arguments: "+wa.getMessage)
				println("          usage: LAUNCHER CONFIG_PATH MIS_PATH")
				println("  with LAUNCHER= java -cp qlmap.jar (or something like that)")
				println("    CONFIG_PATH= path to a configuration file in your mapbase folder")
				println("       MIS_PATH= path to the missions or a folder containing missions")
			}
		}
	}
}