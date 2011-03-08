package de.immaterialien.qlmap

import javax.xml.ws.Provider
import java.io.File
import de.immaterialien.sturmonanny.fbdjhosting._

class TalkingHtmlMissionFilter(args: String) extends HtmlMissionFilter(args)
	with TalkingFilter {
//	var messageCallback : Option[Provider[String]]=None
//	def setMessageCallback(callback : Provider[String])=messageCallback=Some(callback)
	override def afterDone {
		for(
				cb<-messageCallback;
				conf<-mapBase.configuration
				){
			cb invoke conf.finishMessage.apply
		}
	}

//  def invoke(request: Unknown -> TT;): Unknown -> TT; = { null }

}
object TalkingHtmlMissionFilter{
	class Inline(args: String) extends TalkingHtmlMissionFilter(args){
	  override def invoke(file: File): File = {
     	inline(file)
     	afterDone
     	file
    }
	}
}