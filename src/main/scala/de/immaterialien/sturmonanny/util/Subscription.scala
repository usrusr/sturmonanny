package de.immaterialien.sturmonanny.util

import net.liftweb.actor._


package event {
	trait Publication[E, R] { 

  	private case class sub(s: Function[E,R])
	  private case class unsub(s: Function[E,R])

   
    private object subscriptions extends LiftActor {
      protected[Publication] var subscriptions : List[Function[E,R]] = Nil
      def messageHandler = { 
        case sub(s) => subscriptions = subscriptions ::: s ::Nil 
        case unsub(s) => subscriptions = subscriptions - s
      }
    } 
	  
	  def send(event:E) : List[R]= {
	    subscriptions.subscriptions.map(_ apply event)
	  }
    def subscribe(handler : Function[E,R]) : event.Subscription = {
      val ret=new Subscription(handler)
      subscriptions ! sub(handler)
      ret
    }
		class Subscription(protected[event] val handler: Function[E,R]) extends event.Subscription{
			def unsusbcribe = subscriptions ! unsub(handler)
		}
	}
	trait Subscription {
	  def unsusbcribe
	}
 
}