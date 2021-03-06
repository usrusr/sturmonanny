package de.immaterialien.sturmonanny.core

import _root_.de.immaterialien.sturmonanny.util.Logging
/**
 * Armies object provides methods to create holders for variables and values with separate content for each side
 * 
 *  
 * 
 */
object Armies extends Enumeration("None", "Red", "Blue") {
  type Armies = Value
  val None = Value(0)
  val Red = Value(1) 
  val Blue = Value(2) 
 
  object BlueSide extends SideProvider {
    override val currentSide = Blue
  }
  object RedSide extends SideProvider {
    override val currentSide = Red
  } 
  object NeutralSide extends SideProvider {
    override val currentSide = None
  }

  class SidesVar[T](x : => T, private val provider : SideProvider) extends SidesVal[T](x, provider : SideProvider) {
    def update(body : => T):Unit = {
//println("updating "+provider.currentSide+" "+this.getClass.getSimpleName+" to  "+body )
      update(provider, body)
    }
    def update(which:SideProvider, body : => T):Unit = update(which.currentSide, body)
//    def update(which:SideProvider, body : => T):Unit = which currentSide match {
//			case Red => red = body
//	    case Blue => blue = body  
//	    case _ => none = body 
//    }
    def update(which:Armies, body : => T):Unit = which match {
			case Red => red = body
	    case Blue => blue = body  
	    case _ => none = body 
    }
    def value_=(body : =>T) = update(body)
    def other_=(body : =>T) = update(provider.other, body)
  }
  
  sealed class SidesVal[T](x : => T, private val provider : SideProvider) {
	protected var red  : T = x
	protected var blue : T = x 
	protected var none : T = x

		def apply(which:SideProvider) : T = apply(which currentSide)
//    def apply(which:SideProvider) : T = which currentSide match {
//			case Red => red 
//	    case Blue => blue  
//	    case _ => none 
//    }
    def apply(which:Armies.Armies) : T = which match {
			case Red => red 
	    case Blue => blue   
	    case _ => none 
    } 
	
    def apply : T = apply(provider)
    def value = apply
    def other = apply(provider.other)
    
    override def toString = {
      val ret = apply(provider)
      ret match {
        case d:Double => d.formatted("%,.1f")
        case d:Float => d.formatted("%,.1f")
        
        case x=>x.toString
      } 
    }
  }
  def forName(name : String) : Armies = {
    name match {
      case "Red" => Red
      case "Blue" => Blue
      case _ => None
    }
  }
}

trait SideProvider{ 
	def other: SideProvider = currentSide match {
	  case Armies.Red => Armies.BlueSide
	  case Armies.Blue => Armies.RedSide  
	  case Armies.None => Armies.NeutralSide 
	} 
  
	private var internalSide = Armies.None
	private var lastSide = Armies.None
	def currentSide = internalSide
	def previousSide = lastSide
	def currentSide_=(in : Armies.Value) = {
		if(internalSide!=Armies.None) lastSide=internalSide 
		internalSide = in 
	}
	def joinNeutral() = currentSide = Armies.None
	def joinRed() = currentSide = Armies.Red
	def joinBlue() = currentSide = Armies.Blue
	
	def switchSides = internalSide = other.currentSide
 
	object Army {
	  def Val[T](x : => T) = new Armies.SidesVal(x, SideProvider.this)
	  def Var[T](x : => T) = new Armies.SidesVar(x, SideProvider.this)
	}
 
	def apply[T](what:Armies.SidesVal[T]) : T = what.apply 
 
    implicit def fromSidesVal[T](v : Armies.SidesVal[T]):T = v.apply
//    implicit def toStringSidesValDouble(v : Armies.SidesVal[Double]):String = v.apply.formatted("%,.1f")    
//    implicit def toStringSidesValString(v : Armies.SidesVal[String]):String = v.apply    
//    implicit def toStringSidesValOther(v : Armies.SidesVal[_]):String = v.apply.toString    
}