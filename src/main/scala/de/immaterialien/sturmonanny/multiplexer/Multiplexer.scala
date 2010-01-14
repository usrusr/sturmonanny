package de.immaterialien.sturmonanny.multiplexer

import java.net._
import java.io._

//import scala.actors.TIMEOUT
import net.liftweb.actor._
import net.liftweb.common._
import net.liftweb.util.LiftLogger

import scala.collection.mutable.Queue

//import scala.util.logging._

import de.immaterialien.sturmonanny.util._

class Multiplexer(val il2port : Int , val scport : Int) extends LiftActor with TimedLiftActor with Logging{
  
//problem: unangeforderte nachrichten aus dem spiel (chat by player) werden erst mit dem nächsten paket als nachricht erkannt.
//plan: zeilenweise an den actor übergeben, der kann dann in einer kurze timeoutschleife empfangen und beim TIMEOUT broadcasten oder eben nicht
 
  case class DownMessage(val lines: List[List[Byte]]){
    debug("creating downmessage"+ Multiplexer.linesListsToStrings(lines))
  }
  case class DownLine(val line: List[Byte]){
    debug("creating downline"+ Multiplexer.linesListsToStrings(line :: Nil))
    def asMessage() = DownMessage(line::Nil)
  }  
                                                     
  case class UpMessage(val lines: List[List[Byte]], val from : Console)
  case object Close
  val il2actor : LiftActor = this

  val defaultMessageHandler : PartialFunction[Any, Unit] = {
		// default: broadcast as lines
  		case msg : DownLine => {
          debug("v\n"+msg.line)
          for(client <- clients) {
//          debug("v\n"+msg.lines)
          	client ! msg.asMessage
          }
        }
        case msg : UpMessage => {
          // broadcast pending DownLines
          var more = true
          while(more){
            more=false
            onceWithin(10){
              case down : DownLine => for(client <- clients) {
                client ! down.asMessage
                more = true
              }
            }
          }
          
          for(out <- il2out; line : List[Byte]<-msg.lines) {
debug("^\n" +line.toArray)

            out.write( line.toArray )
          }
          // wait for response for 500 ms, after that go back to accepting new UpMessages and broadcasting any
          // unexpected downmessages
          onceWithin(500){
            case down : DownMessage => msg.from ! down
            
          }
        }

        case Close => exit
  }
  
  
  override var messageHandler = defaultMessageHandler

  /**
   *  a client connection, defined with a socket and an accompanying thread listening on the socket's input stream
   */
  class Console(val socket : Socket) extends LiftActor{
    private lazy val stream = socket.getOutputStream
    val reader = socket.getInputStream
    
    val consoleself = this;
    var list : List[Byte] = Nil

    val thread = Multiplexer.daemon{
          //debug("waiting for console stream")
          reader.read match {
            case x if x<0 => Thread.currentThread.interrupt
            case Multiplexer.newline  if list(0)=='\r' => {
              val toReverse : List[Byte] = Multiplexer.newline :: list
              //debug("received newlinee "+toReverse)
              Multiplexer.this ! UpMessage( List(toReverse.reverse), consoleself)
              list = Nil
            }
            case x if x<256 => list = x.toByte :: list
            case _ => Thread.currentThread.interrupt
          }
        }then{
          clients.synchronized{
            clients = clients.remove(x => socket.eq(x.socket))
          }
        }

    def outputStream() : Option[OutputStream] = {
      if(socket.isConnected){
        Some(stream)
      }else{
        None
      }
    }
    override def messageHandler = {
          case msg : UpMessage =>{
            //debug("console up for "+msg.lines)
            msg.lines.map(line=>stream.write(line.toArray))
          }
          case msg : DownMessage =>{
            //debug("console down for "+msg.lines)
            msg.lines.map(line=>stream.write(line.toArray))
          }
    }
  }

  var clients : List[Console] = Nil
  var il2socket : Option[Socket] = None
  var il2out : Option[OutputStream] = None//il2port.getOutputStream
  var il2in : Option[InputStream] = None//il2port.getInputStream
  var listenersocket : Option[ServerSocket] = None

  val serverThread : Thread = Multiplexer.daemon{
    try{
      val actualListenersocket : ServerSocket = listenersocket.getOrElse{
        listenersocket = Some(new ServerSocket(scport))
        listenersocket.get
      }
      val clientconnection : Socket = actualListenersocket.accept
      
      clients.synchronized{
        clients ::= new Console(clientconnection)
      }
    }catch{
      case e : IOException => {
        debug("client listener connection on port "+scport+" failed, retrying ")
        Thread.sleep(1000)
        listenersocket = None
      }
    }
    None
  }then{}


  var il2waiter : Thread = newIl2Waiter

  def newIl2Waiter() : Thread = {
    var il2line : List[Byte] = Nil
    var il2lines : List[List[Byte]] = Nil
    il2in match{
      case Some(instream) => Multiplexer.daemon{
        debug("waiting for instream")
        instream.read match {
          case x if x<0 => Thread.currentThread.interrupt
          case Multiplexer.newline if il2line(0)=='\r' => {
            il2line  = Multiplexer.newline :: il2line
            il2line = il2line.reverse
            il2lines ::= il2line
            debug("line:"+new String(il2line.reverse.toArray) +il2line.reverse)
            val toTest = new String(il2line.toArray).trim
            toTest match {
              case Multiplexer.consoleNPattern(n) => {
                debug("sending "+il2lines.reverse.map(x=>new String(x.toArray)))
                Multiplexer.this ! DownMessage(il2lines.reverse)
                il2lines = Nil
              }
              case _ =>
                     				    debug("appending "+il2lines.reverse.map(x=>new String(x.toArray))+"\nfrom "+il2line.map(x=>(x, ""+new String((x::Nil).toArray)))
                                         +"\ndd not find '"+toTest+"'"+"\n with "+ Multiplexer.consoleNPattern.pattern)
            }

            il2line = Nil
          }
          case x if x<256 => il2line = x.toByte :: il2line
          case _ => Thread.currentThread.interrupt
        }
      }then{
        warn ("lost connection to IL2 instance on port "+il2port+", restarting Multiplexer!")
        il2in = None
        il2out = None
        il2socket.map(_.close)
        il2socket = None
        il2waiter = newIl2Waiter
      }

      case _ => {
        Multiplexer.daemon{
          try{
            val socket = new Socket("192.168.0.3", il2port)
            il2in = Some(socket.getInputStream)
            il2out = Some(socket.getOutputStream)
            il2socket = Some(socket)
            // thread has done its job
            debug("connected to IL2 server on port "+il2port+"")
            Thread.currentThread.interrupt
          }catch{
            case e : IOException => {
              debug("could not connect to IL2 server on port "+il2port+", retrying after a small pause ("+e.getMessage+")")
              Thread.sleep(1000)
            }
          }
        }then{
          //debug("creating new il2waiter with il2in stream "+il2in)
          il2waiter = newIl2Waiter
        }
      }
    }
  }

  def tryinitialize() = {

  }
}
object Multiplexer extends LiftLogger{
  def daemon(body: => Unit): Then = {
    new Then(body)
  }
  class Then(body: => Unit) {

    def then(fin: => Unit) : Thread = {
      val ret : Thread = new Thread{
        override def run() {
          try{
            while( ! Thread.currentThread.isInterrupted ) {
              body
            }
          }catch{
            case e:Exception => debug("Exception in daemon thread: "+e.getClass.getSimpleName+"\n"+e.getMessage)
          }
          fin
        }
      }
      ret.start
      ret
    }
  }

  def linesListsToStrings(l : List[List[Byte]])=   l.map(ll=>new String(ll.toArray))
  
  val consoleNPattern = """<consoleN><(\d+)>""".r
  val newline : Byte = '\n'
  var instance : Option[Multiplexer] = None
  def create(il2port :Int, scport :Int) : Option[Multiplexer] = {
    try{
      //val scsock = new ServerSocket(scport)

      Some(new Multiplexer(il2port, scport))
    }catch{
      case e: Exception => {
        warn("failed to connect "+e.getMessage)

        None
      }
    }
  }

}
