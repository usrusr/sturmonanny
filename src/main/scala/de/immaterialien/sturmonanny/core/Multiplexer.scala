package de.immaterialien.sturmonanny.core

import java.net._

import java.io._

import net.liftweb.actor._
import net.liftweb.common._

import _root_.de.immaterialien.sturmonanny.util._
import _root_.de.immaterialien.sturmonanny.util.event
import _root_.de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema
import de.immaterialien.sturmonanny.util.Daemon._ 
/**
 * manages a server console connection and client console connections, forwarding messages between them 
 * additional work: occasionally a message might be injected from other classes, messages coming from the 
 * IL-2 server are also forwarded to the dispatcher
 * 
 * has to be explicitly started, after Server object is initalized
 *    
 */

class Multiplexer(var host: String, var il2port: Int, var scport: Int) extends TimedLiftActor with Logging with UpdatingMember {
  val timeout = 5000
  object stateFilter extends StateFilter(this)
  def time = server.time
 
  def this(il2port: Int, scport: Int) = this("127.0.0.1", il2port, scport)
  def this(conf: Configuration) = this(conf.server.host.apply, conf.server.il2port.apply, conf.server.consoleport.apply)
  override def updateConfiguration {
    debug("updating mutltiplexer from " + host + ":" + il2port)
    if (conf.server.il2port != il2port || conf.server.host != host) {
      il2port = conf.server.il2port.apply
      host = conf.server.host.apply
      debug("updating mutltiplexer to " + host + ":" + il2port)
      this ! SwitchConnection(host, il2port)
    }
    val newScPort = conf.server.consoleport.apply
    if (newScPort != scport) {
      scport = newScPort
      if (serverThread != null) serverThread interrupt
      else newServerThread
    }
  }

  object il2ConnectionNotifier extends event.Publication[Boolean, Unit] {
    override def subscribe(func: (Boolean => Unit)) = {
      if (il2socket.isDefined) func.apply(true)
      super.subscribe(func)
    }
  }
  //  il2ConnectionNotifier.subscribe(_=>
  //    println("connection!")
  //  )
//  case class DownMessage(val lines: Seq[String])
  	case class DownMessage(val lines: Seq[String]){ 
  		override def toString():String = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(lines).mkString
  	}
  case class DownLine(val line: String) {
    def asMessage() = DownMessage(line :: Nil)
    override def toString() = this.getClass.getSimpleName + ": " + Multiplexer.linesListsToStrings(line :: Nil).mkString
  }

  class DownInternal(val msg: String) extends DownLine(conf.names.tool + ": " + msg)

  class DownPromptLine(override val line: String) extends DownLine(line)

  case class UpMessage(val line: String, val from: AbstractConsole) {
    override def toString() = this.getClass.getSimpleName + ": " + line.trim
  }
  case class addClient(val client: AbstractConsole)
  case class removeClient(val client: AbstractConsole)
  case object Close

  case class SwitchConnection(val host: String, val port: Int)
  case class UpCommand(cms: String)
  class ChatTo(val who: String, val what: String) extends UpCommand("chat " + what + " TO \"" + who + "\"")
  class ChatBroadcast(val what: String) extends UpCommand("chat " + what + " ALL")
  class ChatArmy(val what: String, val army: Armies.Armies) extends UpCommand("chat " + what + " ARMY " + army)
//  class Kick(val who: String) extends UpCommand("kick \"" + who + "\"")

  object promptNotifier;

  //	private[this] def outWrite(line:String):Unit= outWrite(line::Nil)
  protected[this] def outWrite(line: String) {
    outWriteLoadFiltered(stateFilter.loadLine(line))
  }
  private[this] def outWriteLoadFiltered(line: String) {

    for (out <- il2out) {
      var now = server.time.currentTimeMillis
      while (promptTime > now) {
        //debug("OUT: waiting for "+(promptTime-now)+"ms... '"+line+"'")

        promptNotifier.synchronized {
          promptNotifier.wait(timeout)
          //debug("OUT: ...send after waiting "+(server.time.currentTimeMillis-now)+"ms '"+line+"'")
        }
        now = server.time.currentTimeMillis
      }
      promptTime = timeout + now

      out.println(line.trim)
      out.flush
    }
  }

  val defaultMessageHandler: PartialFunction[Any, Unit] = {
    // default: broadcast as lines 
    case SwitchConnection(newhost, newport) => {
      debug("acting conntexction switch")
      clients foreach (_ ! new DownInternal("disconnecting IL-2 instance on " + host + ":" + il2port + " and switching to " + newhost + ":" + newport))
      debug("il2socket:" + il2socket)
      il2socket foreach (_.close)
      debug("il2waiter:" + il2waiter)
      if (il2waiter != null) il2waiter.interrupt
      debug("il2waiter:" + il2waiter)
      il2waiter = newIl2Waiter
      debug("created il2waiter:" + il2waiter)
    }
    case msg: DownLine => {
//handlerLog.write("direct line "+msg)
//println("direct line "+msg)	    	
      //handlerLog.flush
      for (client <- clients) {
        client ! msg //.asMessage
      }
    }
    case msg: UpMessage => if (stateFilter pass msg) {
      //handlerLog.write("beginning processing for "+msg)			
      //handlerLog.flush
      var collectedLines: List[DownLine] = Nil
      // broadcast pending DownLines
      reactOnceWithin(50) {
        case down: DownLine => {
          //handlerLog.write("cleanup broadcast line "+down)			
          //handlerLog.flush
          for (client <- clients) {
            client ! down.asMessage
            extendTimeFromNow(10)
            extendCountFromNow(1)
          }
        }
      }
      outWrite(msg.line)

      // wait for response for 500 ms, after that (or after receiving DownPromptLine) 
      // go back to accepting new UpMessages and broadcasting any unexpected downmessages

      reactWithin(500) {
        case prompt: DownPromptLine => {
          //lines.reverse.foreach(msg.from ! _)
          collectedLines ::= prompt
          val collected = DownMessage(collectedLines.reverse map (_ line))
          msg.from ! collected
          //handlerLog.write("sending "+collectedLines.length+" for "+prompt+"\n++++++collected++++++\n"+collected+"//////\n")					  
          //handlerLog.flush
          collectedLines = Nil
          reactNormally
        }
        case down: DownLine => {

          collectedLines ::= down
          //handlerLog.write("collecting line "+down +"\n total "+collectedLines.size+" lines\n")					  
          //handlerLog.flush
        }
      } andThen {
        //handlerLog.write("leftover "+collectedLines.size+" collected lines: "+collectedLines.mkString("\n ->")+"\n")					  
        //handlerLog.flush

        for (line <- collectedLines.reverse; client <- clients) {
          //handlerLog.write("sending leftover collected line to client "+line+"\n")					  
          //handlerLog.flush
          client ! line
        }
      }

    }

    case UpCommand(cmd) => outWrite(cmd + "\n")
    case Close => exit
    case addClient(client) => {
      clients = clients ::: client :: Nil
    }
    case removeClient(client) => clients.filterNot(client eq _)
    case x: Any => debug("unknown " + x)
  }

  /**
   *  a client connection, defined with a socket and an accompanying thread listening on the socket's input stream
   */
  class Console(val socket: Socket) extends AbstractConsole {
    private lazy val stream = new OutputStreamWriter(socket.getOutputStream)
    val reader = new InputStreamReader(socket.getInputStream)

    //    val consoleself = this;
    val clientline = new StringBuilder

    val thread = Daemon.daemon {
      reader read match {
        case x if x < 0 => Thread.currentThread.interrupt
        case Multiplexer.LF if clientline.last == Multiplexer.CR => {
          clientline append Multiplexer.LF.toChar
          Multiplexer.this ! UpMessage(clientline.toString, Console.this)
          clientline clear
        }
        case x if x < 65536 => {
          clientline append x.toChar
        }
        case _ => Thread.currentThread.interrupt
      }
    } then {
      Multiplexer.this ! removeClient(this)
    }

    def outputStream(): Option[Writer] = {
      if (socket.isConnected) {
        Some(stream)
      } else {
        None
      }
    }
    override def messageHandler = {
      case msg: DownMessage => {
        msg.lines.map(line => stream.write(line))
        stream flush
      }
      case msg: DownLine => {
        stream.write(msg.line)
        stream flush
      }
    }
  }
  /**
   * either a client console or an internal console
   */
  sealed abstract class AbstractConsole extends LiftActor {}
  object pilotsLister extends AbstractConsole {
    private object Requery
    override def messageHandler = {
      case Requery => {
        Multiplexer.this ! UpMessage("user", this)
        Multiplexer.this ! UpMessage("user STAT", this)
        requery(conf.server.pollMillis.apply)
      }
      case DownLine(line) => {
        server.dispatcher ! DispatchLine(line)
      }
      case DownMessage(msg) => {
        val merged = msg.map(_.stripLineEnd) mkString ("\n")
        //debug("to dispatcher msg:\\\n"+merged+"")
        server.dispatcher ! DispatchMessage(merged)
      }
      case _ => // ignore all, this console is only responsible for creating 
    }

    def requery {
      this ! Requery
    }
    def requery(in: Long) {
      LAPinger.schedule(this, Requery, in)
    }
  }
  object eventLogConnection extends AbstractConsole {
    object UpdatedQueues

    var thread: Option[Thread] = None
    var queue: Option[java.util.LinkedList[String]] = None
    override def messageHandler = {
      case UpdatedQueues => { 
      	server.fbdj.fbdj.foreach{fbdj=>
				  queue  = if(fbdj.eventList !=null) Some(fbdj.eventList ) else None

				  
				  thread.foreach(_ interrupt)
      
				  queue.foreach{ list =>
				  	val fos = try{
				  		Some(new java.io.BufferedWriter(new java.io.FileWriter("eventlog.received"))) 					  		
				  	}catch{ case _ => None}				  
				  	val daemon = Daemon.daemon{
				  		list.synchronized{
				  		  while( ! list.isEmpty) {
				  		    val line = list.removeFirst
//debug("from event log line '"+line.trim+"'")
				  		    for(f<-fos)f.append(line+"\r\n")

				  		   	server.eventlog ! DispatchLine(line)
				  		  }
				  		  list.wait(1000)
				  		}
				  	}then{
				  	  thread=None
				  	  for(f<-fos) f.close
				  	}
					  thread = Some(daemon)
				  }
				}
      }
      case _ => // ignore everything, the console-ness of the eventLogConnection is only for thread-handling
    }
  }
  object internalConnection extends AbstractConsole {
    object UpdatedQueues
    var inQueue: Option[java.util.LinkedList[String]] = None
    var outQueue: Option[java.util.LinkedList[String]] = None
    var thread: Option[Thread] = None
    override def messageHandler = {
      case UpdatedQueues => {
        server.fbdj.fbdj.foreach { fbdj =>
          inQueue = if (fbdj.inList != null) Some(fbdj.inList) else None
          outQueue = if (fbdj.outList != null) Some(fbdj.outList) else None

          thread.foreach(_ interrupt)

          inQueue.foreach { list =>
            thread = Some(
              Daemon.daemon {
                list.synchronized {
                  while (!list.isEmpty) {
                    val line = list.removeFirst
                    debug("from fbdj line '" + line.trim + "'")

                    Multiplexer.this ! UpMessage(line, internalConnection)
                  }
                  list.wait(1000)
                }
              } then {
                thread = None
              }
              )
          }
        }
      }
      case DownLine(line) => {
        //debug("to fbdj line '"+line+"'")
        outQueue.foreach { list =>
          list.synchronized {
            list.add(line)
            list.notifyAll
          }
        }
      }
      case DownMessage(msg) => {
        //debug("to fbdj msg:\n'"+msg.map(_ trim).mkString("'\n'")+"'")
        outQueue.foreach { list =>
          list.synchronized {
            msg.foreach(list add _)
            list.notifyAll
          }
        }
      }
      case _ => // ignore all, this console is only responsible for creating 
    }
  }

  var clients: List[AbstractConsole] = internalConnection :: pilotsLister :: Nil
  var il2socket: Option[Socket] = None
  var il2out: Option[PrintWriter] = None
  var il2in: Option[Reader] = None


  
  @volatile
  var promptTime = 0L

  var listenersocket: Option[ServerSocket] = None

  var serverThread: Thread = newServerThread
  def newServerThread: Thread = if (scport == 0) null else Daemon.daemon {

    try {
      val whitelist = conf.server.IPS.apply
    	
      val actualListenersocket: ServerSocket = listenersocket.getOrElse {
        debug("console listening on " + scport)
        
        val whiteSet = whitelist.split(",").map(_ trim).toList

        val onlyLocal = whiteSet.filter(le=> Multiplexer.localIps.pattern.matcher(le.trim).matches).size == whiteSet.size
        val bindAddress = if(onlyLocal) InetAddress.getByName("127.0.0.1") else null
        
        
        
        val serverSocket = new ServerSocket(scport, 0, bindAddress) 
        	
        listenersocket = Some(serverSocket)
        listenersocket.get
      }
      val clientconnection: Socket = actualListenersocket.accept
      Thread.interrupted // accept interrupted us, that's ok, clear
      val remote = clientconnection.getRemoteSocketAddress.asInstanceOf[InetSocketAddress]

      if (!whitelist.trim.isEmpty) { 
        val remoteAdd = remote.getAddress.getHostAddress

        if (!whitelist.contains(remoteAdd)) {
          warn("deniying client connection from " + remoteAdd + ", adress is not whitelisted in configuration/server/ips")
          val stream = clientconnection.getOutputStream
          stream.write(("your ip " + remoteAdd + " is not whitelisted in configuration/server/ips, closing connection").getBytes)
          stream.flush
          clientconnection.close
        }
      }

      if (!clientconnection.isClosed) {
        debug("new client connecting from " + remote.getHostName + " on " + clientconnection.getLocalPort)
        val addMessage = addClient(new Console(clientconnection))
        Multiplexer.this ! addMessage
      }
    } catch {
      case e: IOException => {
        debug("client listener connection on port " + scport + " failed, retrying " + e)
        Thread.sleep(1000)
        listenersocket = None
      }
    }
    None
  } then {
    try {
      listenersocket map (_ close)
    } catch {
      case _ =>
    }
    serverThread = newServerThread
  }

  var il2waiter: Thread = null
  //  def start = il2waiter = newIl2Waiter
  //val instreamWriter = new java.io.FileWriter("il2-output.log")
  def newIl2Waiter(): Thread = {
    debug("creating thread, server is " + server)
    debug("creating thread, dispatcher is " + server.dispatcher)
    var il2line = new StringBuilder
    //instreamWriter.write(""+il2in)  
    il2in match {
      case Some(instream) => Daemon.daemon {
        instream read match {
          case x if x < 0 => Thread.currentThread.interrupt

          case Multiplexer.LF if il2line.last == Multiplexer.CR => {
            il2line append Multiplexer.LF.toChar

            val createdLine = il2line.toString
            //instreamWriter.write("\n<line>"+il2line+"</line>\n")   
            //instreamWriter.flush
            il2line.clear
            createdLine match {
              case Multiplexer.consoleNPattern(n) => {
                promptTime = server.time.currentTimeMillis
                Multiplexer.this.promptNotifier.synchronized {
                  Multiplexer.this.promptNotifier.notifyAll
                }
                Multiplexer.this ! new DownPromptLine(createdLine)
              }
              case _ => {
                Multiplexer.this ! DownLine(createdLine)
                //                server.dispatcher !  DispatchLine(createdLine.stripLineEnd)
              }
            }
          }
          case x if x < 65536 => {
            il2line = il2line append x.toChar
          }
          case _ => Thread.currentThread.interrupt
        }
      } then {
        warn("lost connection to IL2 instance on port " + il2port + ", restarting Multiplexer!")
        il2ConnectionNotifier.publish(false)
        il2in = None
        il2out = None
        il2socket.map(_.close)
        il2socket = None
        il2waiter = newIl2Waiter
      }

      case _ => {
        Daemon.daemon {
          try {
            val socket = new Socket(host, il2port)
            il2in = Some(new InputStreamReader(socket.getInputStream))
            //il2out = Some(new OutputStreamWriter(socket.getOutputStream))
            il2out = Some(new PrintWriter(socket.getOutputStream, true))
            il2socket = Some(socket)

            // thread has done its job
            debug("connected to IL2 server on  " + host + ":" + il2port + "")
            il2ConnectionNotifier.publish(true)
            pilotsLister.requery
            clients foreach (_ ! new DownInternal("connected to IL2 server on  " + host + ":" + il2port + ""))
            Thread.currentThread.interrupt
          } catch {
            case e: IOException => {
              debug("could not connect to IL2 server on port " + il2port + ", retrying after a small pause (" + e.getMessage + ")")
              Thread.sleep(1000)
            }
          }
        } then {
          il2waiter = newIl2Waiter
        }
      }
    }
  }

  def tryinitialize() = {}
  Multiplexer.instancesForShutdown.put(this, None)
}

object Multiplexer extends Logging {
	val localIps = """\Q127.0.0.1\E|\Q0:0:0:0:0:0:0:1\E""".r
//  case object interrupt
//  def daemon(body: => Unit): Then = {
//    new Then(body)
//  }
  
  lazy val instancesForShutdown = {
  	val ret = java.util.Collections.synchronizedMap(new java.util.WeakHashMap[Multiplexer,Option[Int]])
  Runtime.getRuntime.addShutdownHook(
  new Thread("nanny shutdown broadcast"){
  	override def run { import scala.collection.JavaConversions._
println("shutdown hook engaged")
error("shutdown hook engaged")
  		for(instance <- ret.keySet ;
  				writer <- instance.il2out
  				){
println("shutdown hook writing to: "+writer)
error("shutdown hook writing to: "+writer)

  			// we omit synchronisation as this is only an optional emergency measure anyways, if something goes wrong we will have terminated soon enough
  			writer.append("chat sturmonanny disconnecting, might be in active maintenance ALL\n")
  			writer.flush
  		}
println("shutdown hook done")
error("shutdown hook done")
  	
  	}
  })
    	
  	ret
  }

//  class Then(body: => Unit) extends Logging {
//
//    def then(fin: => Unit): Thread = {
//      val ret: Thread = new Thread {
//        override def run() {
//          try {
//            while (!Thread.currentThread.isInterrupted) {
//              body
//            }
//          } catch {
//            case e: Throwable => debug("Exception in daemon thread: " + e.getClass.getSimpleName + "\n" + e.getMessage + "\n" + e.getStackTraceString)
//          }
//          fin
//        }
//      }
//
//      ret.start
//      ret
//    }
//  }

  def linesListsToStrings(l: Seq[String]) = l.map(ll => new String(ll.toArray))

  val consoleNPattern = """<consoleN><(\d+)>\s\s""".r
  val CR: Int = 13
  val LF: Int = 10

  def create(il2port: Int, scport: Int): Option[Multiplexer] = {
    try {
      Some(new Multiplexer(il2port, scport))
    } catch {
      case e: Exception => {
        warn("failed to connect " + e.getMessage)
        None
      }
    }
  }

}
