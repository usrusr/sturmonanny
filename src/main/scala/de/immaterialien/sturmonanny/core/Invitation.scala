package de.immaterialien.sturmonanny.core


import _root_.de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._


class AutoInvitations(pilot:Pilots#Pilot) {
	class InvitationState { 
		def cleaned = { 
			val now = System.currentTimeMillis
			invites = invites.filter(now>_.until)
			this
		}
		private var invites = List[Invite]()
		private var currentInvite :Option[Invitation] = None
		def current = currentInvite
		def clean = currentInvite = None
		
		def in(plane:String):Option[Invitation]={
			invites.find(_.plane.plane == plane).map(new Invitation(_))
		}
		def add(in:Invite)= {
			if( ! pilot.state.flying) pilot.chat("new invitation by "+in)
			invites = in::invites
		}
		def allInvitationsLine = invites match {
      case Nil => None
      case first :: Nil => Some(first.plane + " (with " + first.by + ")")
      case x => Some(x.map(_.plane).mkString(", "))
    }
		class Invitation(val inv:Invite){
			def preaccept() = currentInvite = Some(this)
//				val rules = pilot.domain.server.rules
//				srules.acceptInvitation(pilot.name, inv.by)
//				true
//			}
//			/**
//			 * called when the loadout is switched
//			 * @param newPrice
//			 * @return
//			 */
//			def updatePrice(newPrice:Double):Boolean = if(newPrice>inv.priceLimit && inv.plane.load.isDefined){
//				pilot.chat("loadout too expensive, use "+inv.plane.load+" to get recruited by "+inv.by)
//				currentInvite = None
//				false
//			}else{
//				currentInvite = Some(this)
//				val rules = pilot.domain.server.rules
////				srules.acceptInvitation(pilot.name, inv.by)
//				true
//			}
		}
	}
	private val state = new InvitationState
	def get = state.cleaned
}
case class Invite(by:String, plane:IMarket.Loadout, until:Long, side:Armies.Armies, priceLimit:Double){
	override def toString = by+"("+side+") for "+plane+ " " + ((until - System.currentTimeMillis)/1000)+"s remaining"
}