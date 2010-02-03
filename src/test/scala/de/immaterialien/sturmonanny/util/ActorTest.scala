package de.immaterialien.sturmonanny.util

import de.immaterialien.sturmonanny.util.TimedLiftActor
import net.liftweb.actor.LiftActor

import org.junit.Test

class ActorTest  {
  @Test
  def test(){
     
    case class push() 
    case class one()  
    case class two()  
    

   
    
    class Waiter extends TimedLiftActor {    
          class WaiterFunc extends PartialFunction[Any, Unit] {
      val del : PartialFunction[Any, Unit] = {
        case one() => print("< one\n") 
        case push() => {
           print("push!\n")
          reactWithin(500){
          case two() => print("< two\n")
        }}
      }
        def isDefinedAt(x:Any) = del.isDefinedAt(x)
        def apply(x:Any)  = del.apply(x)
      }
    override val defaultMessageHandler = new WaiterFunc()
//      override val defaultMessageHandler : PartialFunction[Any, Unit] = {
//        case one() => print("< one\n") 
//        case push() => {
//           print("push!\n")
//          reactWithin(500){
//          case two() => print("< two\n")
//        }}
//      }
    }  
    val w = new Waiter()
    print("started") 
    for(i <- 1 to 2){ 
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
