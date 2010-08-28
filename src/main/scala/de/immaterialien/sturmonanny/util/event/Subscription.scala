package de.immaterialien.sturmonanny.util.event

import net.liftweb.actor._

trait Publication[E, R] {

  private case class sub(s: SubscriptionImpl)
  private case class unsub(s: SubscriptionImpl)

  private object subscriptions extends LiftActor {
    protected[Publication] var subscriptions: List[SubscriptionImpl] = Nil
    def messageHandler = {
      case sub(s) => subscriptions = subscriptions ::: s :: Nil
      case unsub(s) => subscriptions = subscriptions filterNot (_ eq s)
    }
  }

  def publish(event: E): List[R] = {
    subscriptions.subscriptions.map(_ handler event)
  }
  def subscribe(handler: Function[E, R]): Subscription = {
    new SubscriptionImpl(handler)
  }
  private class SubscriptionImpl(val handler: Function[E, R]) extends Subscription {
    subscriptions ! sub(this)
    def unsusbcribe = subscriptions ! unsub(this)
  }
}
trait Subscription {
  def unsusbcribe
}

