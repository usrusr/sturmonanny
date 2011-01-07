package de.immaterialien.qlmap

import javax.xml.ws.Provider

class TalkingHtmlMissionFilter(args: String) extends HtmlMissionFilter(args) {
	var messageCallback : Option[Provider[String]]=None
	def setMessageCallback(callback : Provider[String])=messageCallback=Some(callback)
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