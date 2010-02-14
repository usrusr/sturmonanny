package de.immaterialien.sturmonanny.core

import scala.util.parsing.combinator._
/**
 * parses the Chat: --- {pilot}... messages, this would be too much for simple pattern matching
 * 
 * TODO: test if the messages change depending on netmessages.properties, if they do implement dynamic parser setup
 * 
 */
class ServerChatParser extends RegexParsers{
	lazy val statusChat : Parser[PilotEvent] =  "Chat: --- " ~> (pilot <~ " ") ~ whatHappens ^^ {(p) => PilotEvent(p _1, p _2)}
	lazy val pilot : Parser[String] = "(.+)".r
	lazy val whatHappens : Parser[Is.Event]= dies | crashes | returns | joins | leaves
	lazy val dies : Parser[Is.PilotLost] = ( 
		" has beed captured by the opposing force." 
	|	" was killed."
 
 
	|	" spun out!"
	|	" stalls."
	|	" spins out of control."

	|	" did not recover from a spin."
	|	" crashed and burned."
	|	" spins into the ground."
	) ^^^ Is.Dying 
	lazy val crashes : Parser[Is.PlaneLost] = ( 
		" crashes." 
	|	" has crashed."
	|	" just crashed!"
	|	" crashes on landing."
	|	" fails on landing approach."
	|	"'s crate burns on the runway."

	|	" ditches."
	|	" lands in the field."
	|	" successfully crash landed!"

	|	" swims."
	|	" lands on water."
	|	" goes for a swim." 
	|	" walks away from the crash site."
	|	" walks away from the crash site."
	|	" runs away from his burning plane." 
 
	) ^^^ Is.Crashing 
 
 
	lazy val returns : Parser[Is.PlaneSafe] = (
		" lands."
	|	" is on the ground safe and sound."
	|	" is RTB."	)^^^ Is.Returning
	lazy val joins : Parser[Is.Event] = (
		" joins the game."   
    )^^^ Is.Joining
	lazy val leaves : Parser[Is.Event] = (
		" has left the game."
	|	" has been kicked by the host admin."
	|	" has been idle for too long and is kicked."  
	|	" is autokicked by the host."
	|	" is removed from the game."
	|	" has been kicked." 
    )^^^ Is.Leaving
 
 case class PilotEvent(who:String, event:Is.Event)
}

/*
  val hasLeftTheGame = """(.+) has left the game\.""".r
  val joinsTheGame = """(.+) joins the game\.""".r
  val wasKilled = """(.+) was killed\.""".r
  val hasCrashed = """(.+) has crashed\.""".r
  val crashes = """(.+) crashes\.""".r
  val crashes2 = """(.+) just crashed!""".r
  val bailedOut = """(.+) bailed out\.""".r
  val captured = """(.+) has beed captured by the opposing force\.""".r
  val landed1 = """(.+) is RTB\.""".r
  val landed2 = """(.+) is on the ground safe and sound\.""".r
 */

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
	
	case class Flying(val plane : String, val side : Armies.Armies) extends PilotEvent with PlaneEvent
	case class Chatting(val msg : String) extends PilotEvent 
 
	case object Ejecting extends PlaneLost    
 	case object Returning extends PlaneSafe with PilotSafe
	case object Crashing extends PlaneLost
   	case object Dying extends PilotLost with PlaneLost


   	case object Joining extends PilotEvent 
   	case object Leaving extends PilotEvent

   
	case object Destroyed extends PlaneLost with PilotLost
	case class Informed(val text : String) extends PilotEvent
	case object Persisted extends Event
}

/**

	|	" joins the game."
	|	" has left the game."
	|	" has been kicked by the host admin."
	|	" has been idle for too long and is kicked."

	|	" joins the {1} army."
	|	" is ready to go."

	|	" is autokicked by the host."
	|	" is removed from the game."
	|	" has been kicked."

	|	" has been shot down by {0}."
	|	" downs {1}."
	|	" is blown out of the sky by {0}."
	|	" has been destroyed by {0}."
	|	" brings down {1}''s plane."

	|	" is brought down by a direct flak hit."
	|	" is shot down by AAA."
	|	" + flak shell = BOOM!"

	|	" has been killed by artillery."
	|	" catches an artillery round up his tail."
	|	" has been killed by an artillery unit."

	|	" has been killed by a tank."
	|	" has been blown with a tank''s main gun."
	|	" explodes from a tank round."

	|	" has been killed by a ship gunner."
	|	" is brought down by naval AAA."
	|	" out of the sky."

	|	" was shot down by an AI plane."
	|	"."
	|	" loses the fight to an AI plane."

	|	" neatly saws off {1}''s wing."
	|	" deprives {1} of one wing."
	|	" has lost a wing to {0}''s fire."

	|	" blows {1}''s wing off."
	|	"''s wing is blown off with {0}''s nicely placed shot."
	|	"!"

	|	"! {1} is outta here!"
	|	" fills {1} with holes."
	|	" explodes in a million pieces thanks to {0}''s good aim."

	|	" heats things up for {1}."
	|	" scores a direct hit on {1}''s fuel tank."
	|	" sets {1}''s fuel on fire."

	|	" sets {1}''s engine on fire."
	|	" gives {1}''s "cockpit heater" a new meaning."
	|	" is on fire!"

	|	" blows {1}''s tail off."
	|	" saws {1} in half."
	|	"''s rear end is no more."

	|	" is killed by {0}."
	|	" riddles {1}''s body with bullets."
	|	" meets a violent death."

	|	"''s chest is blown open with {0}''s 30mm shell."
	|	" puts a cannon shell through {1}''s body."
	|	"''s chest explodes like an overcooked sausage."

	|	" scores a headshot on {1}!"
	|	" kills {1} with a headshot!"
	|	" beheaded {1}!"

	|	" puts a round right through {1}''s throat!"
	|	" shoots {1} through the neck!"
	|	" chokes on {0}''s bullet!"

	|	" silences {1}''s gunner."
	|	"''s gunner falls to {0}''s fire."
	|	" burdens {1} with a body in the gunner seat."

	|	" kills {1}''s gunner with a headshot!"
	|	" kills {1}''s gunner with a headshot!"
	|	"''s burst punches through {1}''s gunner''s head."

	|	" destroys {1}''s control cables."
	|	" renders {1}''s controls unsuable."
	|	" loses controls to {0}''s fire."

	|	" crashes."
	|	" has crashed."
	|	" just crashed!"

	|	" lands."
	|	" is on the ground safe and sound."
	|	" is RTB."

	|	" crashes on landing."
	|	" fails on landing approach."
	|	"''s crate burns on the runway."

	|	" ditches."
	|	" lands in the field."
	|	" successfully crash landed!"

	|	" swims."
	|	" lands on water."
	|	" goes for a swim."

	|	" is killed in his chute by {0}!"
	|	" shoots {1} hanging in his chute!"
	|	" turns {1} into a heap of meat!"

	|	" spun out!"
	|	" stalls."
	|	" spins out of control."

	|	" did not recover from a spin."
	|	" crashed and burned."
	|	" spins into the ground."

	|	" tastes {0}''s rocket!"
	|	"''s rocket scores a direct hit on {1}!"
	|	" gets a jolt from {0}''s rocket!"

	|	" has been hit by {0}''s bomb!"
	|	" scores a bomb hit on {1}!"
	|	" catches {0}''s bomb as it falls down!"

	|	" walks away from the crash site."
	|	" walks away from the crash site."
	|	" runs away from his burning plane."

	|	" is roasted extra crispy."
	|	" is fried in his burning cockpit."
	|	" burns down with his airplane."

	|	" blows {1} out of control."
	|	" sends {1} spinning out of control."
	|	" hits {1} hard!"

	|	" is definitely a vulcher."
	|	", winners don''t play vulchers."
	|	", you vulcher you!"

	|	" has beed captured by the opposing force."
	|	" bailed out."
	|	" was killed."
 * 
 */