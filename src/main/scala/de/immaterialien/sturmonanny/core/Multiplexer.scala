package de.immaterialien.sturmonanny.core

import java.net._
import java.io._

import net.liftweb.actor._
import net.liftweb.common._

import de.immaterialien.sturmonanny.util._


/**
* manages a server console connection and client console connections, forwarding messages between them 
* additional work: occasionally a message might be injected from other classes, messages coming from the 
* IL-2 server are also forwarded to the dispatcher
* 
* has to be explicitly started, after Server object is initalized
* 
*/  

class Multiplexer(var host : String, var il2port : Int , var scport : Int) extends TimedLiftActor with Logging with UpdatingMember{  

	//  override val messageHandler : PartialFunction[Any, Unit] = {	case x : Any=> debug("ignore"+x)}  

	def this(il2port : Int , scport : Int) = this("127.0.0.1", il2port, scport)
	def this(conf : Configuration) = this(conf.server.host, conf.server.il2port, conf.server.consoleport)
	override def updateConfiguration {
		debug("updating mutltiplexer from " +host+":"+il2port )    
		if(conf.server.il2port != il2port || conf.server.host != host){
			il2port = conf.server.il2port
			host = conf.server.host
			debug("updating mutltiplexer to " +host+":"+il2port )    
			this ! SwitchConnection(host, il2port)
		}
		if(conf.server.consoleport != scport){
			scport = conf.server.consoleport
			serverThread interrupt
		}
	} 

	case class DownMessage(val lines: Seq[String]){
		override def toString() = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(lines).mkString
	}
	case class DownLine(val line: String){
		def asMessage() = DownMessage(line::Nil)
		override def toString() = this.getClass.getSimpleName +": "+Multiplexer.linesListsToStrings(line :: Nil).mkString
	}  

	case class DownInternal(val msg: String) extends DownLine(conf.names.tool +": "+msg)

	case class DownPromptLine(override val line : String) extends DownLine(line)
	
	case class UpMessage(val lines: List[String], val from : AbstractConsole){
		override def toString() = this.getClass.getSimpleName +": "+ Multiplexer.linesListsToStrings(lines).mkString
	}
	case class addClient(val client : AbstractConsole)
	case class removeClient(val client : AbstractConsole)
	case object Close

	case class SwitchConnection(val host : String, val port : Int)
	case class UpCommand(cms:String)
	case class ChatTo(val who : String, val what : String) extends UpCommand("chat "+what+" TO \""+ who+"\"")
	case class ChatBroadcast(val what : String) extends UpCommand("chat "+what+" ALL")
	case class ChatArmy(val what : String, val army : Armies.Armies) extends UpCommand("chat "+what+" ARMY "+army)
	case class Kick(val who : String) extends UpCommand("kick \""+who+"\"")  

	private[this] def outWrite(line:String):Unit= outWrite(line::Nil)
	private[this] def outWrite(lines:Seq[String]){
		for(out<-il2out){
			for(line<-lines) out.append( line )
			out.flush
		}
	}
//val handlerLog = new java.io.FileWriter("handler.log")
	
	val defaultMessageHandler : PartialFunction[Any, Unit] = {
		// default: broadcast as lines 
		case SwitchConnection(newhost, newport) => {
			debug("acting conntexction switch")	      
			clients foreach ( _ ! DownInternal("disconnecting IL-2 instance on "+host+":"+il2port+" and switching to "+newhost+":"+newport))
			debug("il2socket:"+il2socket)	      
			il2socket foreach (_.close)
			debug("il2waiter:"+il2waiter)	      
			if(il2waiter!=null) il2waiter.interrupt
			debug("il2waiter:"+il2waiter)	      
			il2waiter = newIl2Waiter
			debug("created il2waiter:"+il2waiter)	      
		} 
		case msg : DownLine => {
//handlerLog.write("direct line "+msg)		  
//handlerLog.flush
			for(client <- clients) {
				client ! msg//.asMessage
			}
		}
		case msg : UpMessage => {
//handlerLog.write("beginning processing for "+msg)			
//handlerLog.flush
			var collectedLines : List[DownLine] = Nil
		  // broadcast pending DownLines
			reactOnceWithin(50){
				case down : DownLine => {
//handlerLog.write("cleanup broadcast line "+down)			
//handlerLog.flush
					for(client <- clients) {
						client ! down.asMessage
						extendTimeFromNow(10)
						extendCountFromNow(1)
					}
				}
			}
			outWrite(msg.lines)
			
			// wait for response for 500 ms, after that (or after receiving DownPromptLine) 
			// go back to accepting new UpMessages and broadcasting any unexpected downmessages

			reactWithin(500){
				case prompt : DownPromptLine => {
					//lines.reverse.foreach(msg.from ! _)
				  collectedLines ::=prompt
				  val collected = DownMessage(collectedLines.reverse map (_ line))
					msg.from ! collected
//handlerLog.write("sending "+collectedLines.length+" for "+prompt+"\n++++++collected++++++\n"+collected+"//////\n")					  
//handlerLog.flush
					collectedLines = Nil
					reactNormally
				}
				case down : DownLine => {
				  
				  collectedLines ::= down
//handlerLog.write("collecting line "+down +"\n total "+collectedLines.size+" lines\n")					  
//handlerLog.flush
				}
			} andThen {
//handlerLog.write("leftover "+collectedLines.size+" collected lines: "+collectedLines.mkString("\n ->")+"\n")					  
//handlerLog.flush
   
				for(line <- collectedLines.reverse ; client <- clients) {
//handlerLog.write("sending leftover collected line to client "+line+"\n")					  
//handlerLog.flush
				  client ! line
	      }			  
			}
   

		}

		case UpCommand(cmd) => outWrite(cmd+"\n")
		case Close => exit
		case addClient(client) => {
			clients = clients ::: client :: Nil
		}
		case removeClient(client) => clients.remove(x => client eq x)
		case x : Any => debug("unknown "+x)
	}


	/**
*  a client connection, defined with a socket and an accompanying thread listening on the socket's input stream
*/
	class Console(val socket : Socket) extends AbstractConsole{
		private lazy val stream = new OutputStreamWriter(socket.getOutputStream)
		val reader = new InputStreamReader(socket.getInputStream)
		
		//    val consoleself = this;
		val clientline = new StringBuilder

		val thread = Multiplexer.daemon{
			reader read match {
				case x if x<0 => Thread.currentThread.interrupt
				case Multiplexer.LF if clientline.last== Multiplexer.CR => {
					clientline append Multiplexer.LF.toChar
					Multiplexer.this ! UpMessage( List(clientline.toString), Console.this)
					clientline clear
				}
				case x if x<65536  => {
					clientline append x.toChar
				}
				case _ => Thread.currentThread.interrupt
			}
		}then{
			Multiplexer.this ! removeClient(this)
		}

		def outputStream() : Option[Writer] = {
			if(socket.isConnected){
				Some(stream)
			}else{
				None
			}
		}
		override def messageHandler = {
			case msg : DownMessage => {
				msg.lines.map(line=>stream.write(line))
				stream flush
			}
			case msg : DownLine => {
				stream.write(msg.line)
				stream flush
			}
		}
	}
	/**
* either a client console or an internal console
*/
	sealed abstract class AbstractConsole extends LiftActor{
	}
	object pilotsLister extends AbstractConsole {
		private object Requery
		override def messageHandler = {
			case Requery => {
				Multiplexer.this ! UpMessage( "user\r\n"::Nil, this)
				Multiplexer.this ! UpMessage( "user STAT\r\n"::Nil, this)
				requery(conf.server.pollMillis.apply)
			}
			case DownLine(line) => {
				//debug("to dispatcher line '"+line+"'")
				server.dispatcher ! DispatchLine(line)
			}
			case DownMessage(msg) => {
				val merged = msg.map(_.stripLineEnd)mkString("\n")
				//debug("to dispatcher msg:\\\n"+merged+"")
				server.dispatcher ! DispatchMessage(merged)
			}
			case _ => // ignore all, this console is only responsible for creating 
		}
		
		def requery{
			this ! Requery
		}
		def requery(in : Long){
			LAPinger.schedule(this, Requery, in)  
		}
	}

	var clients : List[AbstractConsole] = pilotsLister :: Nil
	var il2socket : Option[Socket] = None
	var il2out : Option[Writer] = None
	var il2in : Option[Reader] = None
	var listenersocket : Option[ServerSocket] = None

	var serverThread : Thread = newServerThread 
	def newServerThread  : Thread = Multiplexer.daemon{
		try{
			val actualListenersocket : ServerSocket = listenersocket.getOrElse{
				listenersocket = Some(new ServerSocket(scport))
				listenersocket.get
			}
			val clientconnection : Socket = actualListenersocket.accept
			val remote = clientconnection.getRemoteSocketAddress.asInstanceOf[InetSocketAddress]
			debug("new client connecting from "+remote.getHostName+" on "+clientconnection.getLocalPort)
			val addMessage = addClient(new Console(clientconnection))
			Multiplexer.this ! addMessage 
		}catch{
			case e : IOException => {
				debug("client listener connection on port "+scport+" failed, retrying ")
				Thread.sleep(1000)
				listenersocket = None
			}
		}
		None
	}then{
		try{
			listenersocket map (_ close)
		}catch{
			case _ =>
		}
		serverThread = newServerThread
	}


	var il2waiter : Thread = null
	//  def start = il2waiter = newIl2Waiter
//val instreamWriter = new java.io.FileWriter("il2-output.log")
	def newIl2Waiter() : Thread = {
		debug("creating thread, server is "+server)
		debug("creating thread, dispatcher is "+server.dispatcher)
		var il2line  = new StringBuilder
//instreamWriter.write(""+il2in)  
		il2in match{
			case Some(instream) => Multiplexer.daemon{
				instream read match {
					case x if x<0 => Thread.currentThread.interrupt
					
					case Multiplexer.LF if il2line.last== Multiplexer.CR => {
						il2line append Multiplexer.LF.toChar
						
						val createdLine = il2line.toString
//instreamWriter.write("\n<line>"+il2line+"</line>\n")   
//instreamWriter.flush
						il2line.clear
						createdLine match {
							case Multiplexer.consoleNPattern(n) => { 
								Multiplexer.this ! DownPromptLine(createdLine)
							}
							case _ =>  {
								Multiplexer.this ! DownLine(createdLine)  
								//                server.dispatcher !  DispatchLine(createdLine.stripLineEnd)
							}
						}
					}
					case x if x < 65536  => {
						il2line = il2line append x.toChar
					}
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
						il2in = Some(new InputStreamReader(socket.getInputStream))
						il2out = Some(new OutputStreamWriter(socket.getOutputStream))
						il2socket = Some(socket)
						// thread has done its job
						debug("connected to IL2 server on  "+host+":"+il2port+"")
						pilotsLister.requery
						clients foreach (_ ! DownInternal("connected to IL2 server on  "+host+":"+il2port+""))
						Thread.currentThread.interrupt
					}catch{
						case e : IOException => {
							debug("could not connect to IL2 server on port "+il2port+", retrying after a small pause ("+e.getMessage+")")
							Thread.sleep(1000)
						}
					}
				}then{
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
	case object interrupt 
	class Then(body: => Unit) extends Logging{

		def then(fin: => Unit) : Thread = {
			val ret : Thread = new Thread {
				override def run() {
					try{
						while( ! Thread.currentThread.isInterrupted ) {
							body
						}
					}catch{
						case e:Exception => debug("Exception in daemon thread: "+e.getClass.getSimpleName+"\n"+e.getMessage+"\n"+e.getStackTraceString)
					}
					fin
				}
			}
			ret.start
			ret
		}
	}

	def linesListsToStrings(l : Seq[String])=   l.map(ll=>new String(ll.toArray))

	val consoleNPattern = """<consoleN><(\d+)>\s\s""".r
	val CR : Int = 13
	val LF : Int = 10

	def create(il2port :Int, scport :Int) : Option[Multiplexer] = {
		try{
			Some(new Multiplexer(il2port, scport))
		}catch{
			case e: Exception => {
				warn("failed to connect "+e.getMessage)
				None
			}
		}
	}  


}
