package de.immaterialien.sturmonanny

import org.mortbay.jetty.Connector
import org.mortbay.jetty.Server
import org.mortbay.jetty.webapp.WebAppContext


object Start {
System.err.println("INIT START OBJ")  
  def main(args : Array[String]) : Unit = {

	
  val server = new Server(8080)
  val context = new WebAppContext()
  
  context.setServer(server)
  context.setContextPath("/")
  context.setWar("src/main/webapp") 

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
