package de.immaterialien.sturmonanny.persistence

import de.immaterialien.sturmonanny._
import net.liftweb.actor.LiftActor
import net.liftweb.common.Box

class BalanceWrapper extends IBalanceDao with core.UpdatingMember with util.Log{
  def load(pilot:String):Option[IBalanceDao.BalanceRB] = { 
  	val ret = actor !! (Msg.LoadReq(pilot), 500)
  	ret.asA[Msg.LoadRes].flatMap(_.res)
  }
	def store(pilot:String, balanceRed:Option[Double], balanceBlue:Option[Double]) {
		actor ! Msg.Store(pilot, balanceRed, balanceBlue)
	}
	def open(props:Map[String, String], info:String=>Unit, error:String=>Unit) = actor ! Msg.Open(props)
	def close(info:String=>Unit, error:String=>Unit) = actor ! Msg.Close
	
	var dao : Option[IBalanceDao] = None
	
	case class Last(cls : String, props : Map[String, String])
	var last = Last(null, null)
	override def updateConfiguration {
		val next = Last(
				server.conf.persistence.implementation.apply, 
				server.conf.persistence.properties.map
		)
		if(dao.isEmpty || next != last) {
			close(log.debug(_), log.error(_))
			if(next.cls != last.cls) dao = try Some{
				val newCls = classOf[BalanceWrapper]
						.getClassLoader.loadClass(next.cls)
						.asInstanceOf[Class[IBalanceDao]]
				newCls.newInstance
			}catch{ case e => {
				log.error("could not load '"+next.cls+"' ", e)
				None 
			}}
			open(next.props, log.debug(_), log.error(_)) 
			last = next
		}
	}
	/**
	 * 	override def getPrice(plane : IMarket.Loadout) : Double = {
		!!(Msg.getPrice(plane), 500)
	  		.asA[Msg.getPriceResult].getOrElse(Msg.getPriceResult(0d))
	  		.price
	}
	 * @author ulf
	 *
	 */
	private object actor extends LiftActor { 
		def messageHandler = { 
			case Msg.LoadReq(pilot) => {
				val ret = dao flatMap (_.load(pilot))
				reply(Msg.LoadRes(ret))
			}
			case Msg.Store(pilot, balanceRed, balanceBlue) => dao map (_.store(pilot, balanceRed, balanceBlue))
			case Msg.Open(props) => dao map (_ open(props, log.debug(_), log.error(_)))
			case Msg.Close => {
				dao map (_ close(log.debug(_), log.error(_)))
				dao = None
			}
			case _ =>
		}
	}
}
private object Msg {
	case class LoadReq(pilot:String)
	case class LoadRes(res:Option[IBalanceDao.BalanceRB])
	case class Store(pilot:String, balanceRed:Option[Double], balanceBlue:Option[Double])
	case class Open(props:Map[String, String])
	case object Close
}