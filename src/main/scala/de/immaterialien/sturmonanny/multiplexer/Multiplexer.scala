package de.immaterialien.sturmonanny.multiplexer

import java.net._
import java.io._

import scala.actors._
import scala.actors.Actor._

import scala.util.logging._  
//import _root_.java.lang.{Runnable, Thread};


class Multiplexer(val il2port : Int , val scport : Int) extends ConsoleLogger{
   /**
    *  a client connection
    */
   case class Console(val socket : Socket, val thread : Thread){
     private lazy val stream = socket.getOutputStream
     
     def outputStream() : Option[OutputStream] = {
       if(socket.isConnected){
         Some(stream)
       }else{
         None
       }
     }
   }
   
	var clients : List[Console] = Nil
	var il2socket : Option[Socket] = None 
	var il2out : Option[OutputStream] = None//il2port.getOutputStream
	var il2in : Option[InputStream] = None//il2port.getInputStream
	var listenersocket : Option[ServerSocket] = None
 
	val serverThread : Thread = MultiplexerO.daemon{
    	try{
	    	val actualListenersocket : ServerSocket = listenersocket.getOrElse{
	    		listenersocket = Some(new ServerSocket(scport))
	    		listenersocket.get
	    	} 
	    	val clientconnection : Socket = actualListenersocket.accept  
			val reader = clientconnection.getInputStream
			var list : List[Byte] = Nil
			clients.synchronized{
				clients ::= Console(clientconnection, MultiplexerO.daemon{
					reader.read match {
					  case x if x<0 => Thread.currentThread.interrupt
					  case MultiplexerO.newline => {
					    val toReverse : List[Byte] = MultiplexerO.newline :: list
					    up(toReverse.reverse)
					  	list = Nil
					  }
					  case x if x<256 => list = x.toByte :: list   
					  case _ => Thread.currentThread.interrupt
					}
		        }then{
		        	clients.synchronized{
		        	  clients = clients.remove(x => clientconnection.eq(x.socket))
		        	}
	            }) 
	        }
    	}catch{
    	  case e : IOException => {
    	    log("client listener connection on port "+scport+" failed, retrying ")
    	    Thread.sleep(1000)
    	    listenersocket = None
    	  }
    	}
		None
	}then{}
 
	var il2line : List[Byte] = Nil
	var il2waiter : Thread = newIl2Waiter
 
	def newIl2Waiter() : Thread = {
	  il2in match{
	    case Some(instream) => MultiplexerO.daemon{
	    	
				instream.read match {
					  case x if x<0 => Thread.currentThread.interrupt
					  case MultiplexerO.newline => {
					    val toReverse : List[Byte] = MultiplexerO.newline :: il2line
					    broadcast(toReverse.reverse)
					  	il2line = Nil
					  }
					  case x if x<256 => il2line = x.toByte :: il2line   
					  case _ => Thread.currentThread.interrupt
					}
			}then{
				log("lost connection to IL2 instance on port "+il2port+", restarting Multiplexer!")
				il2in = None
				il2out = None
				il2socket.map(_.close)
				il2socket = None
				il2waiter = newIl2Waiter
			}
	    
   	    case _ => {
	      MultiplexerO.daemon{
	        try{
	          val socket = new Socket("192.168.0.3", il2port)
	          il2in = Some(socket.getInputStream)
	          il2out = Some(socket.getOutputStream)
	          il2socket = Some(socket)
	          // thread has done its job
	          log("connected to IL2 server on port "+il2port+"")
	          Thread.currentThread.interrupt
	        }catch{
	          case e : IOException => {
	            log("could not connect to IL2 server on port "+il2port+", retrying after a small pause ("+e.getMessage+")")
	            Thread.sleep(1000)
	          }
	        }
	      }then{
	        il2waiter = newIl2Waiter
	      }
	    }
      }
    }
 
 
	def up(in : Seq[Byte]) : Unit = synchronized{
		in match{
		  case a : Array[Byte] => il2out.map(_.write(a))
		  case _ => il2out.map(_.write(in.toArray));
		}
	}
 	def broadcast(in : Seq[Byte]) : Unit = synchronized{
 	    def broadcast(in : Array[Byte]) : Unit = {
 	    	for(client <- clients){
 	    		client.outputStream.map(_.write(in))
 	    	}
 	    }
		in match{
		  case a : Array[Byte] => broadcast(a)
		  case _ => broadcast(in.toArray);
		}
	}
  def tryinitialize() = {}
}
object MultiplexerO extends ConsoleLogger{
   def daemon(body: => Unit): Then = {
     new Then(body)
   }
   class Then(body: => Unit){
	   protected def stop = Thread.currentThread.interrupt
       def then(fin: => Unit) : Thread = {
    	  val ret : Thread = new Thread{
    	   	override def run() {
    	   		try{
	    	   		while( ! Thread.currentThread.isInterrupted ) {
	    	   			body
	    	   		}
    	   		}catch{
    	   		  case e:Exception => log("exception in daemon thread: "+e.getMessage) 
    	   		}
    	   		fin
    	   	}
        	}
        ret.start
        ret
       }
   }
  
  
  val newline : Byte = '\n'
  var instance : Option[Multiplexer] = None
  def create(il2port :Int, scport :Int) : Option[Multiplexer] = {
    try{ 
     	//val scsock = new ServerSocket(scport)

    	Some(new Multiplexer(il2port, scport))
    }catch{
      case e: Exception => {
        log("failed to connect "+e.getMessage)
        
        None
      }
      
    }
    
    
    
  }
  
}
