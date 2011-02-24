package de.immaterialien.sturmonanny.fbdjhosting
import java.io.File
import _root_.de.immaterialien.sturmonanny.core
import _root_.de.immaterialien.sturmonanny.util
import org.apache.commons.exec._

object DcgMissionProvider extends util.Log{  
  val misFilter = new java.io.FilenameFilter(){
    override def accept(dir:File, name:String) = name.endsWith(".mis")
	}
  
  def newMisFilter(oldSet:Set[String]) = new java.io.FilenameFilter(){
    override def accept(dir:File, name:String) = name.endsWith(".mis") && ! oldSet.contains(name)
	}
  
  def listMissions(searchPath:File):Set[String] = {
		val list = searchPath.list(misFilter);
                  
  	if(list!=null) {
  		Set[String]() ++ list.toList
  	} else {
  		 log.error ("failed to list in "+searchPath.getAbsolutePath)
  		 Set[String]()
  	}
	}
} 

class DcgMissionProvider(conf : core.Configuration) extends javax.xml.ws.Provider[File] with util.Log{
  def dcgPathAndCommand(searchPath:File = new File(".")) = {
        // use configured dcgPath or the mission path
    val dcgPath = if(conf.fbdj.DCG.dcgPath.apply.isEmpty) searchPath else new File(conf.fbdj.DCG.dcgPath.apply)
    val commandLine = conf.fbdj.DCG.dcgCommand.apply
    (dcgPath, commandLine)
  }
  override def invoke(oldMissionPath:File):File = {
    def searchPath = oldMissionPath.getParentFile
    
    val beforeFiles = DcgMissionProvider.listMissions(searchPath)
    
//    // use configured dcgPath or the mission path
//    val dcgPath = if(conf.fbdj.DCG.dcgPath.apply.isEmpty) searchPath else new File(conf.fbdj.DCG.dcgPath.apply)
//    val commandLine = conf.fbdj.DCG.dcgCommand.apply
    val (dcgPath, commandLine)=dcgPathAndCommand(searchPath)
    log.info("starting DCG \n "+commandLine+"\n  at\n "+dcgPath.getAbsolutePath)
    
    val cmd = CommandLine.parse(commandLine) 
    
    val executor = new DefaultExecutor()
    try{
    	executor.setWorkingDirectory(dcgPath)
    }catch{
    	case x:ExecuteException=>{
    		log.warning("DCG process returned "+x.getMessage)
    	}
    }
    val exitValue = executor.execute(cmd)
    
    
//    val exec = Runtime.getRuntime.exec(commandLine, null, dcgPath);
//    
//    def consumeAll(stream:java.io.InputStream, desc:String){
//    	val sb = new StringBuilder
//    	def print = if(sb.length>0) log.info("from DCG process "+desc+": "+sb.toString)
//    	util.Daemon.named(desc){ 
//    		var read = stream.read
//    		while(read > -1){ 
//println(desc+" got "+read)    			
//    			if(read=='\n' || read=='\r'){
//    				print
//    				sb.clear
//    			} else {
//    				sb.append(read.toChar)
//    			}
//    			read = stream.read
//    		}
//    	} then {
//    		print
//    		log.info("DCG "+desc+" done")
//    	}
//    }
//    consumeAll(exec.getInputStream, "stdout")
//    consumeAll(exec.getErrorStream, "stderr")
//
//    exec.waitFor
    
    val afterList = searchPath.list(DcgMissionProvider.newMisFilter(beforeFiles))
    
    val ret = if(afterList.length==1) new File(searchPath, afterList(0))
    else {
   	 		if(afterList.length==0) throw new IllegalArgumentException("DCG terminated but no new mission was found in "+searchPath)
log.warning("multiple DCG missions: "+afterList)       
        val selected = afterList.foldLeft((oldMissionPath,0L)){(previous, fname) =>
          val file = new File(searchPath, fname)
          if(file.exists && file.lastModified>previous._2) {
					  (file, file.lastModified)
					}else{
					  previous
					}
        }._1
log.warning("multiple DCG missions: selected "+selected)       
        
        selected 
    }
    
    ret
  }
  override def toString = "DCG mission generator at "+dcgPathAndCommand()
}
