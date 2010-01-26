package de.immaterialien.sturmonanny.multiplexer


/**
 * Armies object provides methods to create holders for variables and values with separate content for each side
 * 
 * 
 * 
 */
object Armies extends Enumeration("Red", "Blue", "None") {
  type Armies = Value
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

  class SidesVar[T](x : => T, private val provider : SideProvider) extends SidesVal[T](x, provider : SideProvider) {
    def update(body : => T):Unit = update(provider, body)
    def update(which:SideProvider, body : => T):Unit = which currentSide match {
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

    def apply(which:SideProvider) : T = which currentSide match {
		case Red => red 
	    case Blue => blue  
	    case _ => none 
    } 
    def apply : T = apply(provider)
    def value = apply
    def other = apply(provider.other)
  }
}

trait SideProvider{
	def other: SideProvider = currentSide match {
	  case Armies.Red => Armies.BlueSide
	  case Armies.Blue => Armies.RedSide  
	  case Armies.None => Armies.NeutralSide 
	} 
  
	private var internalSide = Armies.None
	def currentSide() = internalSide
	def currentSide_=(in : Armies.Value) = internalSide = in

	def switchSides = internalSide = other.currentSide
 
	object Army {
	  def Val[T](x : => T) = new Armies.SidesVal(x, SideProvider.this)
	  def Var[T](x : => T) = new Armies.SidesVar(x, SideProvider.this)
	}
 
	def apply[T](what:Armies.SidesVal[T]) : T = what.apply 
 
    implicit def fromSidesVal[T](v : Armies.SidesVal[T]):T = v.apply
}