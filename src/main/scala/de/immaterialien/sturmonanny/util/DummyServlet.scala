package de.immaterialien.sturmonanny.util

import javax.servlet._
import http._

/**
 * a workaround for the lack of a <load-on-startup> equivalent in servlet filters, 
 * will boot the instances even if the Lift filter does not boot at startup 
 */
class DummyServlet extends HttpServlet {
		override def destroy {}
		override def getServletConfig:ServletConfig = null
		override def getServletInfo:String=null
		override def init(arg0:ServletConfig ) {
//System.err.println("dummy servlet forcing init") 
			val instances = global.Instances.configuration
			if((" "+instances).isEmpty) throw new RuntimeException("should never happen, but should still force full evaluation of the instance configuration")
//System.err.println("dummy servlet started") 
    }
		override def service(arg0:HttpServletRequest, arg1:HttpServletResponse) {
		  arg1.sendError(HttpServletResponse.SC_NOT_FOUND,"Dummy Servlet.")
		}
//System.err.println("dummy servlet loaded")    
}