package de.immaterialien.sturmonanny

import _root_.org.mortbay.jetty.Server
import _root_.org.mortbay.jetty.webapp.WebAppContext


object Main{
   def main(args : Array[String]) {
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