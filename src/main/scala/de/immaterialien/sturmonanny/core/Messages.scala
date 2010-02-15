package de.immaterialien.sturmonanny.core


case class PilotMessage(who: String, event : Is.Event)
case class DispatchLine(line:String)
object Is {  
    
	trait Event 
	trait PlaneEvent extends Event
	trait PilotEvent extends Event
	trait Positive extends Event
	trait Negative extends Event
	trait PlaneLost extends PlaneEvent with Negative
	trait PlaneSafe extends PlaneEvent with Positive
	trait PilotLost extends PilotEvent with Negative
	trait PilotSafe extends PilotEvent with Positive 
	
	case class Flying(val plane : String, val side : Armies.Armies) extends PilotEvent with PlaneEvent {
	  def this(plane:String, side:String) = this(plane, Armies.forName(side )) 
	}
	case class Chatting(val msg : String) extends PilotEvent 
 
	case object Ejecting extends PlaneLost    
 	case object Returning extends PlaneSafe with PilotSafe
	case object Crashing extends PlaneLost
   	case object Dying extends PilotLost with PlaneLost


   	case object Joining extends PilotEvent 
   	case object Leaving extends PilotEvent

    case object Unknown extends Event 
    case object Ignored extends Event 
   
//	case object Destroyed extends PlaneLost with PilotLost
	case class Informed(val text : String) extends PilotEvent
	case object Persisted extends Event
}