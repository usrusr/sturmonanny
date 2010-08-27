package de.immaterialien.sturmonanny.fbdjhosting
import java.io.File
import _root_.de.immaterialien.sturmonanny.core
import _root_.de.immaterialien.sturmonanny.util

object DcgMissionProvider {
  val misFilter = new java.io.FilenameFilter(){
    override def accept(dir:File, name:String) = name.endsWith(".mis")
	}
  
  def newMisFilter(oldSet:Set[String]) = new java.io.FilenameFilter(){
    override def accept(dir:File, name:String) = name.endsWith(".mis") && ! oldSet.contains(name)
	}
  
  def listMissions(searchPath:File):Set[String] = {
		val list = searchPath.list(misFilter);
                  
  	Set[String]() ++ list.toList
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
    val exec = Runtime.getRuntime.exec(commandLine, null, dcgPath);
    
    exec.waitFor
    
    val afterList = searchPath.list(DcgMissionProvider.newMisFilter(beforeFiles))
    
    val ret = if(afterList.length==1) new File(searchPath, afterList(0))
    else {
   	 		if(afterList.length==0) throw new IllegalArgumentException("DCG terminated but no new mission was found in "+searchPath)
       
        afterList.foldLeft((oldMissionPath,0L)){(previous, fname) =>
          val file = new File(searchPath, fname)
          if(file.exists && file.lastModified>previous._2) {
					  (file, file.lastModified)
					}else{
					  previous
					}
        }._1
    }
    
    ret
  }
  override def toString = "DCG mission generator at "+dcgPathAndCommand()
}
