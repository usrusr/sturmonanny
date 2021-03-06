package de.immaterialien.sturmonanny.core



class Message
case class PilotMessage(who: String, event : Is.Individual, where:At.Location) extends Message
object PilotMessage {
	def apply(who: String, event : Is.Individual):PilotMessage = apply(who, event, At.Nowhere)
}
//case class PilotMessageAt(override val who : String, override val event : Is.Individual, where:At.Location) extends PilotMessage(who, event)


case class GlobalMessage(event : Is.Global) extends Message

case class DispatchLine(line:String) 
case class DispatchMessage(message:String) {
//	val stack = new Exception().getStackTraceString
}

object At {
	trait Location
   case class Coordinate(x:Double, y:Double) extends Location
   case object Nowhere extends Location
}
object EventSource {
	sealed trait Source
	case class Logfile(msg:Is.Event) extends Source
	case class Console(msg:Is.Event) extends Source
	case class UserState(msg:Is.Event) extends Is.Individual with Source// special marker wrapper for the very slow user STAT states
}

object Is {  
    
	sealed trait Event
	sealed trait Global extends Event
 	
  case object MissionBegin extends Global
  case object MissionEnd extends Global
  case class MissionChanging(mis:java.io.File) extends Global
	
 
	sealed trait Individual extends Event
	trait PlaneEvent extends Individual
	trait PilotEvent extends Individual
	trait Positive extends Event
	trait Negative extends Event
	trait PlaneLost extends PlaneEvent with Negative
	trait PlaneSafe extends PlaneEvent with Positive
	trait PilotLost extends PilotEvent with Negative
	trait PilotSafe extends PilotEvent with Positive 
	trait PilotState extends PilotEvent
	
 	case object LandedAtAirfield extends PilotState 
 	case object KIA extends PilotState with PilotLost
 	case object InFlight extends PilotState

 	
 	case object HitTheSilk extends PilotState with PlaneLost
 	case object Selecting extends PilotState  
 
	case class InPlaneForSide(val plane : String, val side : Armies.Armies) extends PilotEvent with PlaneEvent {
	  def this(plane:String, side:String) = this(plane, Armies.forName(side )) 
	}
	/**
	 * either coming from refly menu or switching seats inside a plane
	 */
  case class TakingSeat(plane : String, position:Int) extends PilotEvent
  case class Loading(plane : String, weapon:String, fuel:Double) extends PilotEvent
  
 
	case class Chatting(val msg : String) extends PilotEvent 
 
	case object Ejecting extends PlaneLost    
 	case object Returning extends PlaneSafe with PilotSafe
	case object Crashing extends PlaneLost
   	case object Dying extends PilotLost with PlaneLost
   	/**
   	 * event log version of died
   	 */
   	case class Killed(seat:Int) extends PilotLost with PlaneLost
    


   	case object Joining extends PilotEvent 
   	case object Leaving extends PilotEvent

    case object Unknown extends Individual with Global 
    case object Ignored extends PilotState with Global 
   
//	case object Destroyed extends PlaneLost with PilotLost
	case class Informed(val text : String) extends PilotEvent
	case object Persisted extends Event
}