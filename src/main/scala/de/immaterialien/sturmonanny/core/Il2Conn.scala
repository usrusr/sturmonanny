package de.immaterialien.sturmonanny.core

import java.net._
import java.io._

import net.liftweb.actor._
import net.liftweb.common._
import java.util.concurrent
import java.nio

/**
 * a clean, non-duplex connection between the actor-world and the il2 host
 */
/*
object Il2Conn {
  case class To(host:String, port:Int)
  case class Start
  case class Stop
}
class Il2Conn(val multi:Multiplexer) extends de.immaterialien.sturmonanny.util.Log{
  var conn = Il2Conn.To("", 0)
  private val queue = new concurrent.ConcurrentLinkedQueue()

  object actor  extends LiftActor {
    def messageHandler = {
      case nConn:Il2Conn.To => if(conn!=nConn) ConnThread reconnect conn
      case Multiplexer#UpMessage() => 
    }
  }
  object ConnThread extends Runnable {
    @volatile private var recon : Option[Il2Conn.To] = None
    private object notifier 
    private var thread :Thread= null
    private var stopped = false
    @volatile var write : Option[Send] = None
    @volatile var read : Option[List[String]] = None
    def wake {
      notifier.synchronized{
        notifier notifyAll
      }
    }
    /**
     * 
     */
    def sleep(millis:long) {
      try{
      	notifier.synchronized{notifier.wait(millis)}
      }catch{
        case e : InterruptedException => stopped = true
        
      }
    }

	  
    def reconnect(n:Il2Conn.To){
      recon = Some(n)
      wake
    }
    def open(optto: Option[Il2Conn.To]):Option[Streams]={
   		var ret : Streams = null
   		optto foreach {to=>
	      try{
	        val socket = new Socket()
          val addr=new java.net.InetSocketAddress(to.host, to.port)
	        while(undisturbed){
            log.debug("trying to connect to IL2 on "+addr)
	      	  socket.connect(addr, 1000)
	        }
	      	if(socket.isConnected){
	      	  log.info("connected to IL2 on "+addr)
	      	  ret = new Streams(socket)
          }
	      }catch{
	        case _ => ret = null
	      }
      }
      if(ret==null || ! ret.sock.isConnected) None else Some(ret)
    }
    class Streams(val sock: Socket){

      val in = new BufferedReader(new InputStreamReader(sock.getInputStream, "UTF-8"))
      val out = new OutputStreamWriter(sock.getOutputStream, "UTF-8")
      val il2line  = new StringBuilder
      private def readIl2Line(timeout:Int):String = {
        il2line.clear
        var done=false
        sock.setSoTimeout(timeout)
        while(undisturbed && !done) {
      	  
          try{
	          in.read() match  {
					    case x if x<0 => {
					    	stopped = true
					    }
				    	case Multiplexer.LF if il2line.last== Multiplexer.CR => {
					    	il2line append Multiplexer.LF.toChar
					    	done=true
					    }
							case x if x < 65536  => {
								il2line append x.toChar
							}         
				    	case _ => Thread.currentThread.interrupt
		        }
          }catch{
            case ioe:InterruptedIOException=> () 
          }
        }
	    	val createdLine = il2line.toString
	    	il2line.clear
	    	createdLine
      }
      def readAnswerLines(req:String):List[String] = {
        out.append(req)
        out.append("\r\n")
        
	    	var lines :List[String]= Nil
        var done=false
        while(undisturbed && !done) {
          val line=readIl2Line(500)
          lines=line::lines
          done = line match {
            case Multiplexer.consoleNPattern(n) => true
            case _ => false
          }
        }
        lines.reverse
      }
    }
    
    
		var socket : Option[Streams] = None 
    def undisturbed = socket.isDefined && ! recon.isDefined && ! stopped
		def run {
		  thread = Thread.currentThread
		  var current : Option[Il2Conn.To] = if(Il2Conn.this.conn.host.isEmpty) None else Some(Il2Conn.this.conn)
  
			while(!stopped){
			  if(recon.isDefined) try {
			    current = recon
			    recon = None
			    socket foreach (_.sock.close)
			    socket = None
			  } catch{ 
			    case e => e.printStackTrace 
			  }
			  if(!current.isDefined && !stopped){
			    log.debug("waiting for connection data")
			    sleep(10000)
			  }
			  try{
				  socket = current map { to =>
				    new Streams(new Socket(to.host, to.port))
				  }
				  while(undisturbed) socket foreach {il2=>
				  	write foreach {req=>
				  		val answer = il2.readAnswerLines(req.line)
				  	  req.rec ! answer
				  	}
          
				    sleep(1000)				    
				  }
			  }catch{
			    case e => e.printStackTrace  
			  }
			}
		}
	}
}
*/