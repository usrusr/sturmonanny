package de.immaterialien.sturmonanny.dcg


import javax.xml.ws.Provider
import java.io.File
import de.immaterialien.sturmonanny.fbdjhosting._
import de.immaterialien.sturmonanny.util._

import java.io.File
import java.io.IOException
import java.lang.management.ManagementFactory
import scala.collection.JavaConversions._ 


object SelfUpdateFilter {
	val waitTime = """\s*(?:waitTime\s*=\s*)?(\d+)\s*""".r
}
class SelfUpdateFilter(configuration:String) extends Provider[File] with TalkingFilter with Log{ import SelfUpdateFilter._
	val waitBeforeShutdown = configuration match {
		case waitTime(s)=> s.toInt
		case _ => 5000
	}
	
	val (restartCommand, jarFiles, started, basedir) = {
        val cmd = new StringBuilder();
        val mxbean = java.lang.management.ManagementFactory.getRuntimeMXBean
        cmd.append(System.getProperty("java.home") + File.separator + "bin" + File.separator + "java ");
        for (jvmArg <- mxbean.getInputArguments) {
            cmd.append(jvmArg + " ");
        }
        val classPathString = mxbean.getClassPath
        cmd.append("-cp ").append(classPathString).append(" ")
        cmd.append("de.immaterialien.sturmonanny.StartAssembly")

        val base=new File(".").getAbsoluteFile
        val list = classPathString.split(File.pathSeparatorChar).map(fn=>new File(base, fn)).toList
        
        (cmd.toString, list, mxbean.getStartTime, base)
	}
log.debug("SelfUpdateFilter: restartCommand:"+restartCommand+ " jarFiles:"+jarFiles+" started:"+started+" basedir:"+basedir)			
	
	def mostRecentJar:Long = jarFiles.map(_ lastModified).max
	
	

	override def invoke(file: File): File = {
		var lastMod = mostRecentJar
log.debug("SelfUpdateFilter: lastMod:"+lastMod+ " started:"+started)		
		if(lastMod>started){
			var nextMod = 0
			for(cb <- messageCallback) cb.invoke("sturmonanny code updated, preparing restart")
			
			/**
			 * wait for files to stabilize, 
			 * and for fbdj to accept stats update,
			 */
			while(nextMod != lastMod){
				Thread.sleep(waitBeforeShutdown)
				val nextMod = mostRecentJar
//				val restartTime = System.currentTimeMillis
log.debug("SelfUpdateFilter: lastMod:"+lastMod+ " nextMod:"+nextMod)		
				lastMod=nextMod
			}
			new File(file.getAbsolutePath+".nannyupdate").createNewFile
			for(cb <- messageCallback) cb invoke "restarting now!"
log.debug("SelfUpdateFilter: restartCommand:"+restartCommand)					
			Runtime.getRuntime.exec(restartCommand)
log.debug("SelfUpdateFilter: bye!")					
			System.exit(0)
		}

		file
	}

}