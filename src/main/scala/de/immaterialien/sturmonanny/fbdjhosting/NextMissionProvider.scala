package de.immaterialien.sturmonanny.fbdjhosting

import java.io.File
import _root_.de.immaterialien.sturmonanny.core
import _root_.de.immaterialien.sturmonanny.util
import javax.xml.ws.Provider
import scala.collection.JavaConversions

object NextMissionProvider 
	extends de.immaterialien.sturmonanny.util.Log 
	{ 
	class MessageCallback(server:core.Server) extends Provider[String]{
		override def invoke(msg:String):String = {
			server.multi ! new server.multi.ChatBroadcast(msg)
			msg
		}
	}
  def makeList(conf:core.Configuration, server:core.Server):List[Provider[File]] = if(conf==null) Nil else{
  	
    val ret = new java.util.TreeMap[Int, Provider[File]]
    lazy val messageCallback = new MessageCallback(server)
    ret.put(0, new DcgMissionProvider(conf))
    val addons = conf.fbdj.DCG.addons
    val allAddons = addons.map
    for(nameNum <- allAddons){
      val num = nameNum._2
      val cName = nameNum._1
      val actualClass = cName.replace("_", ".").replace("..","_")
      if(ret.containsKey(num)){
        log.error("A mission processor ("+ret.get(0).getClass.getCanonicalName+") already exists at position "+num+", ignoring "+cName)
      }else{
        var instance:Provider[File] = null
        val arg = conf.fbdj.DCG.addonArguments(cName)
        try{
       	 	val cls = Class.forName(actualClass).asInstanceOf[Class[Provider[File]]]
          
          try{
            val constructor = cls.getConstructor(classOf[String])
            instance = constructor.newInstance(arg).asInstanceOf[Provider[File]]
          }catch{
            case _ => {
              val constructor = cls.getConstructor()
              instance = constructor.newInstance().asInstanceOf[Provider[File]]
            } 
          }
      	  type withCallback = {def setMessageCallback(callback : Provider[String]):Unit}
          if(instance.isInstanceOf[withCallback]) {
          	instance.asInstanceOf[withCallback].setMessageCallback(messageCallback)
          }
          
      	  if(instance!=null) ret.put(num, instance)
        }catch{
          case x => log.error("could not load mission processor "+actualClass+" for processor position "+num+":", x)
      	case _ =>
        }
      } 
    }

  	val jclRet = JavaConversions.asBuffer(new java.util.ArrayList[Provider[File]](ret.values())).toList
  	
println("jcl ret "+jclRet+"\n from "+ret)  	
  	jclRet
  }
}

class NextMissionProvider(private var filters:List[Provider[File]], server:core.Server) extends javax.xml.ws.Provider[File] with util.Log{ 
	//def this(conf:core.Configuration)=this(NextMissionProvider.makeList(conf))
	def this(server:core.Server)=this(NextMissionProvider.makeList(server.conf, server), server)
	def updateConfiguration(conf:core.Configuration) = filters=NextMissionProvider.makeList(conf,server)

  override def invoke(oldMissionPath:File):File = {
    var ret = oldMissionPath
println("mission callback filters: "+filters)    
    for(filter<-filters){
      try{
println("invoking "+filter.getClass.getSimpleName+" with input "+ret)
log.debug("invoking "+filter.getClass.getSimpleName+" with input "+ret)      	
      	val next = filter.invoke(ret)
log.debug("got "+next+" from "+filter.getClass.getSimpleName)      	
      	if(next!=null) ret = next
      }catch{
        case x => {
        	log.error("error in mission processor "+filter.getClass.getCanonicalName+", skipping ", x)
        	x.printStackTrace()
        }
      	case _ =>
      }
    }
    
    ret
  }
}
