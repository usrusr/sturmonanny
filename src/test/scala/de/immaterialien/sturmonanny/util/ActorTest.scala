package de.immaterialien.sturmonanny.util

import de.immaterialien.sturmonanny.util.TimedLiftActor
import net.liftweb.actor.LiftActor

class ActorTest  {
  def main(args : Array[String]) {
     
    case class push() 
    case class one()  
    case class two()  
    
    class Waiter extends LiftActor with TimedLiftActor {    
      override val defaultMessageHandler : PartialFunction[Any, Unit] = {
        case one() => print("< one\n") 
        case push() => {
           print("push!\n")
          reactWithin(500){
          case two() => print("< two\n")
        }}
      }
    }  
    val w = new Waiter()
    print("started") 
    for(i <- 1 to 20){ 
      print("one >\n")
      w ! one()
       w ! push()
      Thread.sleep(100)
      print("two >\n")
      w ! two()
      Thread.sleep(100)
    }
    
    None
  }
}
