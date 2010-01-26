package de.immaterialien.sturmonanny.multiplexer

import java.net._
import java.io._

import net.liftweb.actor._
import net.liftweb.common._

import de.immaterialien.sturmonanny.util._
  import de.immaterialien.sturmonanny.multiplexer._
 
class Multiplexer(var host : String, var il2port : Int , var scport : Int) extends TimedLiftActor with Logging with UpdatingMember{
  
  def this(il2port : Int , scport : Int) = this("127.0.0.1", il2port, scport)
  def updateConfiguration {
    if(conf.server.il2port != il2port || conf.server.host != host){
      il2port = conf.server.il2port
      this ! SwitchConnection(conf.server.host, conf.server.il2port)
      
    } 
  } 
   
  case class DownMessage(val lines: Seq[Seq[Byte]]){
    override def toString() = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(lines).mkString
  }
  case class DownLine(val line: Seq[Byte]){
    def asMessage() = DownMessage(line::Nil)
    override def toString() = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(line :: Nil).mkString
  }  
  
  case class DownInternal(val msg: String) extends DownLine((conf.server.toolName +": "+msg).getBytes)
  
  case class DownPromptLine(override val line : List[Byte]) extends DownLine(line)
                                                       
  case class UpMessage(val lines: List[List[Byte]], val from : Console){
    override def toString() = this.getClass.getSimpleName +": "+ Multiplexer.linesListsToStrings(lines).mkString
  }
  case class addClient(val client : Console)
  case class removeClient(val client : Console)
  case object Close
  val il2actor : LiftActor = this
  case class SwitchConnection(val host : String, val port : Int)
  case class ChatTo(val who : String, val what : String){
    def this(pilot : Pilots#Pilot, what :String) = this(pilot.name, what)
  }
  
//override val defaultMessageHandler : PartialFunction[Any, Unit] = {case x => debug("ignore"+x)}  
  override val defaultMessageHandler : PartialFunction[Any, Unit] = {
		// default: broadcast as lines 
	    case SwitchConnection(newhost, newport) => {
	      clients foreach ( _ ! DownInternal("disconnecting IL-2 instance on "+host+":"+il2port+" and switching to "+newhost+":"+newport))
	      il2socket foreach (_.close)
	      il2waiter.interrupt
	      il2waiter = newIl2Waiter
	    } 
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
        case ChatTo(who, what) => il2out foreach (_ write ("CHAT "+what+" TO "+ who).getBytes)
        case Close => exit
        case addClient(client) => clients ::: client :: Nil
        case removeClient(client) => clients.remove(x => client eq x)
        case x : Any => debug("unknown "+x)
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
          Multiplexer.this ! removeClient(this)
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
      
      Multiplexer.this ! addClient(new Console(clientconnection))
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
            val socket = new Socket(host, il2port)
            il2in = Some(socket.getInputStream)
            il2out = Some(socket.getOutputStream)
            il2socket = Some(socket)
            // thread has done its job
            debug("connected to IL2 server on  "+host+":"+il2port+"")
            clients foreach (_ ! DownInternal("connected to IL2 server on  "+host+":"+il2port+""))
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

  def linesListsToStrings(l : Seq[Seq[Byte]])=   l.map(ll=>new String(ll.toArray))
  
  val consoleNPattern = """<consoleN><(\d+)>""".r
  val newline : Byte = '\n'

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
