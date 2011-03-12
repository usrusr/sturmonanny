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
	val headLastDotRest = """^(.*)\.([^\.]*)$""".r 
 	def loadClassAndTryInnverVariants(actualClass:String):Class[_]={
		log.debug("trying to load "+actualClass)
 		try {
 			Class.forName(actualClass)
 		}catch {
 			case x:ClassNotFoundException => {
 				actualClass match {
 					case headLastDotRest(head, tail) => loadClassAndTryInnverVariants(head+"$"+tail)
 					case _ => throw x
 				}
 			}
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
        	
       	 	val cls = loadClassAndTryInnverVariants(actualClass).asInstanceOf[Class[Provider[File]]]
          
          try{
            val constructor = cls.getConstructor(classOf[String])
            instance = constructor.newInstance(arg).asInstanceOf[Provider[File]]
          }catch{ 
          	case x : NoSuchMethodException => {
//x.printStackTrace         
							try{
								val constructor = cls.getConstructor()
								instance = constructor.newInstance().asInstanceOf[Provider[File]]
							}catch{ 
								case y => { 
									x.printStackTrace
								}
            	}
          	}
            case x : java.lang.reflect.InvocationTargetException => { 
//x.printStackTrace         
							try{
								val constructor = cls.getConstructor()
								instance = constructor.newInstance().asInstanceOf[Provider[File]]
							}catch{ 
								case y => { 
									x.printStackTrace
								}
            	}
            } 
            case x => { 
x.printStackTrace
            }
          }
      	  //type withCallback = {def setMessageCallback(callback : Provider[String]):Unit}
          //if(instance.isInstanceOf[{def setMessageCallback(callback : Provider[String]):Unit}]) {
          try{
	          val method = instance.getClass.getMethod("setMessageCallback", classOf[Provider[_]])
	          
	          if(method!=null) {
	          	//instance.asInstanceOf[{def setMessageCallback(callback : Provider[String]):Unit}].setMessageCallback(messageCallback)
	          	method.invoke(instance, messageCallback)
	          }
          }catch{case _ =>}
      	  if(instance!=null) ret.put(num, instance)
        }catch{
          case x => {
x.printStackTrace          	
          	log.error("could not load mission processor "+actualClass+" for processor position "+num+":", x)
          }
      	case _ =>
        }
      } 
    }

  	val jclRet = JavaConversions.asBuffer(new java.util.ArrayList[Provider[File]](ret.values())).toList
  	
println("jcl ret "+jclRet+"\n from "+ret)  	
  	jclRet
  }
}
sealed trait MissionMetaProcessor extends javax.xml.ws.Provider[File] with util.Log{
	def filteredFilters : Iterable[javax.xml.ws.Provider[File]]
	def inlineAndParallel : (List[Provider[File]],List[Provider[File]])
  override def invoke(oldMissionPath:File):File = {
    var ret = oldMissionPath

    val (inline,parallel) = inlineAndParallel 
println("mission callback filters: "+(inline,parallel))    
    
    
    for(filter<-inline){ 
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
    util.Daemon.once("parallel mission postprocessing"){
    	var dtemp = ret
	    for(filter<-parallel){ 
	      try{
	println("invoking (in parallel) "+filter.getClass.getSimpleName+" with input "+dtemp)
	log.debug("invoking (in parallel) "+filter.getClass.getSimpleName+" with input "+dtemp)      	
	      	val next = filter.invoke(dtemp)
//	log.debug("got "+next+" from "+filter.getClass.getSimpleName)      	
	      	if(next!=null) dtemp = next
	      }catch{
	        case x => {
	        	log.error("error in parallel mission processor "+filter.getClass.getCanonicalName+", skipping ", x)
	        	x.printStackTrace()
	        }
	      	case _ =>
	      }
	    }    	
    }.then()
    
    ret
  }	
}

	/**
	 * implementation mixin for the setMessageCallback contract 
	 * (which is based on plain reflection and javax.xml.ws.Provider[String] so that java implementations without extra dependencies are possible)  
	 * 
	 * @author ulf
	 */
	trait TalkingFilter {
		var messageCallback : Option[Provider[String]]=None
		def setMessageCallback(callback : Provider[String])=messageCallback=Some(callback)
	}
	/**
	 * a donothing filter that is meant to tell the filter processor 
	 * to break processing, return the current state and branch out a 
	 * thread for all subsequent filters
	 * 
	 * @author ulf
	 */
	class ParallelReturn extends Provider[File] with NonMutatingFilter{
		override def invoke(file: File): File = file
	}	
	trait NonMutatingFilter
class NextMissionProvider(private var filters:List[Provider[File]], server:core.Server) extends MissionMetaProcessor { 
	//def this(conf:core.Configuration)=this(NextMissionProvider.makeList(conf))
	def this(server:core.Server)=this(NextMissionProvider.makeList(server.conf, server), server)
	def updateConfiguration(conf:core.Configuration) = filters=NextMissionProvider.makeList(conf,server)

	object onlyNonMutating extends MissionMetaProcessor {
		def filteredFilters = filters.filter(_.isInstanceOf[NonMutatingFilter])
		def inlineAndParallel : (List[Provider[File]],List[Provider[File]]) = NextMissionProvider.this.inlineAndParallel
	}
	def filteredFilters = filters
	 
	def inlineAndParallel : (List[Provider[File]],List[Provider[File]])={
		val base = filteredFilters
		val inline = if(base.isEmpty) base else {
			base.takeWhile( ! _.isInstanceOf[ParallelReturn])
		}
		val parallel=if(base.isEmpty) base else {
			val rest = base.dropWhile( ! _.isInstanceOf[ParallelReturn])
			if(rest.isEmpty) rest else rest.tail
		}
		(inline,parallel)
	}

	
//	def inlineFilters = {
//		if(base.isEmpty) base else {
//			filteredFilters.takeWhile( ! _.isInstanceOf[ParallelReturn])
//		}
//	}
//	def parallelFilters = {
//		val base = filteredFilters
//	}
	
}
