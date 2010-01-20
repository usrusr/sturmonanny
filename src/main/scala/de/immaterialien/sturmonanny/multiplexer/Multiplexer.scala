package de.immaterialien.sturmonanny.multiplexer

import java.net._
import java.io._

import net.liftweb.actor._
import net.liftweb.common._

import de.immaterialien.sturmonanny.util._
 
class Multiplexer(val il2port : Int , val scport : Int) extends TimedLiftActor with Logging{
   
  case class DownMessage(val lines: List[List[Byte]]){
    override def toString() = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(lines).mkString
  }
  case class DownLine(val line: List[Byte]){
    def asMessage() = DownMessage(line::Nil)
    override def toString() = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(line :: Nil).mkString
  }  
  case class DownPromptLine(override val line : List[Byte]) extends DownLine(line)
                                                     
  case class UpMessage(val lines: List[List[Byte]], val from : Console){
    override def toString() = this.getClass.getSimpleName +": "+ Multiplexer.linesListsToStrings(lines).mkString
  }
  case object Close
  val il2actor : LiftActor = this

  override val defaultMessageHandler : PartialFunction[Any, Unit] = {
		// default: broadcast as lines
  		case msg : DownLine => {
          for(client <- clients) {
          	client ! msg.asMessage
          }
        }
        case msg : UpMessage => {
          // broadcast pending DownLines
            reactOnceWithin(10){
              case down : DownLine => for(client <- clients) {
                client ! down.asMessage
                extendTimeFromNow(10)
                extendCountFromNow(1)
              }
            }
          
          for(out <- il2out; line : List[Byte]<-msg.lines) {
            out.write( line.toArray )
          }
          // wait for response for 500 ms, after that (or after receiving DownPromptLine) go back to accepting new UpMessages and broadcasting any
          // unexpected downmessages
          var lines : List[DownLine] = Nil
          reactWithin(500){
            case down : DownPromptLine => {
              lines.reverse.foreach(msg.from ! _)
              msg.from ! down
              lines = Nil
              reactNormally
            }
            case down : DownLine => {
              lines ::= down
            }
          }
          for(line <- lines.reverse ; client <- clients) client ! line
        }

        case Close => exit
  }
  

  /**
   *  a client connection, defined with a socket and an accompanying thread listening on the socket's input stream
   */
  class Console(val socket : Socket) extends LiftActor{
    private lazy val stream = socket.getOutputStream
    val reader = socket.getInputStream
    
    val consoleself = this;
    var list : List[Byte] = Nil

    val thread = Multiplexer.daemon{
          reader.read match {
            case x if x<0 => Thread.currentThread.interrupt
            case Multiplexer.newline  if list(0)=='\r' => {
              val toReverse : List[Byte] = Multiplexer.newline :: list
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
//          case msg : UpMessage => 	msg.lines.map(line=>stream.write(line.toArray))
          case msg : DownMessage => msg.lines.map(line=>stream.write(line.toArray))
          case msg : DownLine => 	stream.write(msg.line.toArray)
    }
  }

  var clients : List[Console] = Nil
  var il2socket : Option[Socket] = None
  var il2out : Option[OutputStream] = None
  var il2in : Option[InputStream] = None
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
    il2in match{
      case Some(instream) => Multiplexer.daemon{
        instream.read match {
          case x if x<0 => Thread.currentThread.interrupt
          case Multiplexer.newline if il2line(0)=='\r' => {
            il2line  = Multiplexer.newline :: il2line
            il2line = il2line.reverse
            val toTest = new String(il2line.toArray).trim
            toTest match {
              case Multiplexer.consoleNPattern(n) => 
                Multiplexer.this ! DownPromptLine(il2line)
              case _ => 
                Multiplexer.this ! DownLine(il2line)
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
object Multiplexer extends Logging{
  def daemon(body: => Unit): Then = {
    new Then(body)
  }
  class Then(body: => Unit) extends Logging{

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
  object minutes extends Thread with LiftActor {
//    import scala.collection.mutable
//    import java.{util => mutable}
    import scala.collection._
    object minute
    private case class add(val who : LiftActor)
    private case class remove(val who : LiftActor)

    def unsubscribe( who : LiftActor) {
      this ! add(who)
    }
    def subscribe( who : LiftActor) {
      this ! remove(who)
    }
    override def run = while(! this.isInterrupted ){
       Thread.sleep(60*1000)
	   this ! minute
    }
    val multiplexers :mutable.Set[LiftActor]= (new jcl.WeakHashMap(new java.util.WeakHashMap)).keySet
    def messageHandler = {
      case add(who) => multiplexers += who
      case remove(who) => multiplexers -= who
      case minute => multiplexers map ( _ ! minute) 
    }
    this setDaemon true
    this start
  }
}
