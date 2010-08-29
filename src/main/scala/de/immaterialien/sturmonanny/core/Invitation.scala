package de.immaterialien.sturmonanny.core


import _root_.de.immaterialien.sturmonanny.util._
import scala.collection.mutable
import scala.util.matching._


class AutoInvitations {
	private val state = new InvitationState
	def get = state.cleaned
}

class InvitationState { import InvitationsState._
	def cleaned = {
		val now = System.currentTimeMillis
		invites = invites.filter(now>_.until)
		this
	}
	private var invites = List[Invitation]()
}
object InvitationsState {
	case class Invitation(by:String, plane:IMarket.Loadout, until:Long, side:Armies.Armies) 
}