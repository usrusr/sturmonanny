package de.immaterialien.sturmonanny.fbdjhosting

import java.io.File
import de.immaterialien.sturmonanny.core
import de.immaterialien.sturmonanny.util
import javax.xml.ws.Provider
import scala.collection.JavaConversions

object NextMissionProvider 
//	extends de.immaterialien.sturmonanny.util.Log 
	{   
  def makeList(conf:core.Configuration):List[Provider[File]]={
    val ret = new java.util.TreeMap[Int, Provider[File]]
    
    ret.put(0, new DcgMissionProvider(conf))
    
    for(nameNum <- conf.fbdj.DCG.addons.map){
      val num = nameNum._2
      val cName = nameNum._1
      
      if(ret.containsKey(num)){
//        log.error("A mission processor ("+ret.get(0).getClass.getCanonicalName+") already exists at position "+num+", ignoring "+cName)
      }else{
        var instance:Provider[File] = null
        val arg = conf.fbdj.DCG.addonArguments(cName)
        try{
       	 	val cls = Class.forName(cName).asInstanceOf[Class[Provider[File]]]
          
          try{
            val constructor = cls.getConstructor(classOf[String])
            instance = constructor.newInstance(arg).asInstanceOf[Provider[File]]
          }catch{
            case _ => {
              val constructor = cls.getConstructor()
              instance = constructor.newInstance().asInstanceOf[Provider[File]]
            } 
          }
      	  
      	  if(instance!=null) ret.put(num, instance)
        }catch{
//          case x => log.error("could not load mission processor "+cName+" for processor position "+num+":", x)
      	case _ =>
        }
      }
    }

  	JavaConversions.asBuffer(new java.util.ArrayList[Provider[File]](ret.values())).toList
  }
}

class NextMissionProvider(filters:List[Provider[File]]) extends javax.xml.ws.Provider[File] with util.Log{
	def this(conf:core.Configuration)=this(NextMissionProvider.makeList(conf))
  override def invoke(oldMissionPath:File):File = {
    var ret = oldMissionPath
    
    for(filter<-filters){
      try{
      	val next = filter.invoke(ret)
      	if(next!=null) ret = next
      }catch{
//        case x => log.error("error in mission processor "+filter.getClass.getCanonicalName+", skipping ", x)
      	case _ =>
      }
    }
    
    ret
  }
}
