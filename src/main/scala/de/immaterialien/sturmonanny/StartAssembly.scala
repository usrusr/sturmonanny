package de.immaterialien.sturmonanny


import org.mortbay.jetty
import jetty.Connector
import jetty.Server
import jetty.webapp.WebAppContext
import javax.servlet.http._

object StartAssembly {
	net.lag.configgy.Configgy.configure("log.conf")
	val log = net.lag.logging.Logger.get
   
  def main(args : Array[String]) : Unit = {

		val warUrl = this.getClass.getClassLoader().getResource("hosts.html");
	
	  val external = warUrl.toExternalForm  
	 
	 	val globals = global.GlobalConfig.singleton 
	 
	 	val port : Int= globals.jetty.port.apply
	  val server = new Server(port)
	  val conn = server.getConnectors()(0)

    
	  val context = new WebAppContext(external, "/")
	  
	  context.setServer(server)
	  
	  val ips = globals.jetty.IPS.apply
	  if( ! ips.trim.isEmpty){
	    val old = server.getHandler
	    var handler : jetty.handler.AbstractHandler = new jetty.handler.AbstractHandler(){
	      override def handle(pathInContext:String , request:HttpServletRequest , response:HttpServletResponse, number:Int ){
	        request match {
	          case req:jetty.Request=> if( ! ips.contains(request.getRemoteAddr)){
	         	 	response.sendError(HttpServletResponse.SC_FORBIDDEN, "IP "+request.getRemoteAddr+" not whitelisted in global.config/jetty/ips")
	         	 	req.setHandled(true)
	          }else{
	            if(old!=null)old.handle(pathInContext,request, response, number)
	          }
	          case _ => {
	         	 	response.sendError(HttpServletResponse.SC_FORBIDDEN, "unknown request type "+request.getClass.getCanonicalName)
	          }
	        }
	      }
	    }
	    server.removeHandler(old)
	    server.addHandler(handler)
	  ()}
	  
	  context.addServlet(classOf[de.immaterialien.sturmonanny.util.DummyServlet], "/*")
	  context.addFilter(classOf[net.liftweb.http.LiftFilter], "/*", jetty.Handler.ALL)
	  server.addHandler(context)
	
	  try {
	    println(">>> STARTING EMBEDDED JETTY SERVER, PRESS ANY KEY TO STOP");
	    server.start();
	    while (System.in.available() == 0) {
	      Thread.sleep(5000)
	    }
	    server.stop()
	    server.join() 
	  } catch {
	    case exc : Exception => {
	      exc.printStackTrace() 
	      System.exit(100) 
	    }
	  }
  }
}
