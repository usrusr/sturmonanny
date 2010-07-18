package de.immaterialien.sturmonanny

import org.mortbay.jetty
import jetty.Connector
import jetty.Server
import jetty.webapp.WebAppContext

object StartAssembly {
  def main(args : Array[String]) : Unit = {

	val warUrl = this.getClass.getClassLoader().getResource("hosts.html");
//println("warUrl: '"+warUrl+"'")    

 val external = warUrl.toExternalForm  
//println("external: '"+external+"'")
 
 	val globals = global.GlobalConfig.singleton  
 
 	val port : Int= globals.jetty.port.apply
  val server = new Server(port)
  val context = new WebAppContext(external, "/")
  
  context.setServer(server)
 // context.setContextPath("/")
 // context.setWar("src/main/webapp")
  
  /*
<servlet>
  <servlet-name>dummy404</servlet-name>
  <servlet-class>de.immaterialien.sturmonanny.util.DummyServlet</servlet-class>
  <load-on-startup>2</load-on-startup>
</servlet>
<filter>
  <filter-name>LiftFilter</filter-name>
  <display-name>Lift Filter</display-name>
  <description>The Filter that intercepts lift calls</description>
  <filter-class>net.liftweb.http.LiftFilter</filter-class>
</filter>
  	
<servlet-mapping>
  <servlet-name>dummy404</servlet-name>
  <url-pattern>/ *</url-pattern>
</servlet-mapping>
<filter-mapping>
  <filter-name>LiftFilter</filter-name>
  <url-pattern>/ *</url-pattern>
</filter-mapping>  
*/
                   
	//val sHolder = new jetty.servlet.ServletHolder(new de.immaterialien.sturmonanny.util.DummyServlet)
  context.addServlet(classOf[de.immaterialien.sturmonanny.util.DummyServlet], "/*")
  
  //val fHolder = new jetty.servlet.FilterHolder(new net.liftweb.http.LiftFilter)
  context.addFilter(classOf[net.liftweb.http.LiftFilter], "/*", jetty.Handler.ALL)
  //context.set
  
  


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
