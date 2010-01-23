package de.immaterialien.sturmonanny.multiplexer

object Army extends Enumeration("Red", "Blue", "None") {
  type Army = Value
  val Red = Value 
  val Blue = Value
  val None = Value
  
  object BlueSide extends SideProvider {
    override val currentSide = Blue
  }
  object RedSide extends SideProvider {
    override val currentSide = Red
  }
  object NeutralSide extends SideProvider {
    override val currentSide = None
  }
 
  def Var[T](x : => T) = new SidesVar[T](x)
  def Val[T](x : => T) = new SidesVal[T](x)

  class SidesVar[T](x : => T) extends SidesVal[T](x) {
    def update(which : SideProvider, body : => T) = which currentSide match {
		case Red => red = body
	    case Blue => blue = body  
	    case _ => none = body
    }
  }
  
  sealed class SidesVal[T](x : => T) {
	protected var red  : T = x
	protected var blue : T = x 
	protected var none : T = x

    def apply(which : SideProvider) = which currentSide match {
		case Red => red 
	    case Blue => blue 
	    case _ => none 
    }
  }
  
  trait SideProvider {
	  def currentSide : Army.Value
  }
}
trait SideProvider {
  def currentSide : Army.Value
}

trait Sided extends Army.SideProvider{
	private var internalSide = Army.None
	override def currentSide() = internalSide
	def currentSide_=(in : Army.Value) = internalSide = in
	def other: Army.SideProvider = currentSide match {
	  case Army.Red => Army.BlueSide
	  case Army.Blue => Army.RedSide
	  case Army.None => Army.NeutralSide 
	}
	def switchSides = internalSide = other.currentSide
}