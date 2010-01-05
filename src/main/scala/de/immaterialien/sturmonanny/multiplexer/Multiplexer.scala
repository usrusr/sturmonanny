package de.immaterialien.sturmonanny.multiplexer

import java.net._
import java.io._

import scala.actors._
import scala.actors.Actor._

import scala.util.logging._  
//import _root_.java.lang.{Runnable, Thread};


class Multiplexer(val il2port : Socket , val scport : ServerSocket) extends ConsoleLogger{

   
   
   case class Console(val socket : Socket, val thread : Thread){
     lazy val ouputStream = socket.getOutputStream
   }
   
	var clients : List[Console] = Nil
 
	val il2out = il2port.getOutputStream
	val il2in = il2port.getInputStream
 
	val serverThread : Thread = daemon{
	  
		val clientconnection = scport.accept
		val reader = clientconnection.getInputStream
		var list : List[Byte] = Nil
		clients.synchronized{
			clients ::= Console(clientconnection, daemon{
				reader.read match {
				  case x if x<0 => Thread.currentThread.interrupt
				  case Multiplexer.newline => {
				    val toReverse : List[Byte] = Multiplexer.newline :: list
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
		None
	}then{}
 
	var il2line : List[Byte] = Nil
	val il2waiter = daemon{
		il2in.read match {
				  case x if x<0 => Thread.currentThread.interrupt
				  case Multiplexer.newline => {
				    val toReverse : List[Byte] = Multiplexer.newline :: il2line
				    broadcast(toReverse.reverse)
				  	il2line = Nil
				  }
				  case x if x<256 => il2line = x.toByte :: il2line   
				  case _ => Thread.currentThread.interrupt
				}
	}then{
		log("lost connection to IL2 instance, restarting Multiplexer!")
		MultiplexerO.instance = MultiplexerO.create(il2port.getPort, scport.getLocalPort, 0)
	}

 
 
	def up(in : Seq[Byte]) : Unit = synchronized{
		in match{
		  case a : Array[Byte] => il2out.write(a)
		  case _ => il2out.write(in.toArray);
		}
	}
 	def broadcast(in : Seq[Byte]) : Unit = synchronized{
 	    def broadcast(in : Array[Byte]) : Unit = {
 	    	for(client <- clients){
 	    		client.ouputStream.write(in)
 	    	}
 	    }
		in match{
		  case a : Array[Byte] => broadcast(a)
		  case _ => broadcast(in.toArray);
		}
	}
  def tryinitialize()
}
object MultiplexerO extends ConsoleLogger{
   def daemon(body: => Unit): Then = {
     new Then(body)
   }
   class Then(body: => Unit){

       def then(fin: => Unit) : Thread = {
    	  val ret : Thread = new Thread{
    	   	override def run() {
    	   		while( ! Thread.currentThread.isInterrupted ) {
    	   			body
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
    	val il2sock = new Socket("localhost", il2port)
     	val scsock = new ServerSocket(scport)

    	Some(new Multiplexer(il2sock, scsock))
    }catch{
      case e: Exception => {
        log("failed to connect "+e.getMessage)
        
        daemon{
        	create(il2port, scport)
        }then{
          
        }
        None
      }
      
    }
    
    
    
  }
  
}
