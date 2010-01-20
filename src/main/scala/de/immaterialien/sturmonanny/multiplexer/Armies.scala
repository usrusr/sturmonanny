package de.immaterialien.sturmonanny.multiplexer

object Army extends Enumeration("Red", "Blue", "None") {
  type Army = Value
  val Red, Blue, None = Value

  def Var[T](x : => T) = new SidesVar[T](x)
  def Val[T](x : => T) = new SidesVal[T](x)
  
//  abstract class Sides[T]{
//    def blue : T
//    def red  : T
//   
//	  def in(which : Value) = which match  {
//	    case Red => red
//	    case Blue => blue
//	  }
//  }
//  
  class SidesVar[T](x : => T) 
//  extends Function1[Sided, Function1[Any, T]]
  {
	var blue : T = x 
	var red  : T = x
	var none  : T = x

	def set(which : Sided)(body : => T) : T = set(which.currentSide)(body)
	def set(which : Value)(body : => T) : T= which match  {
		case Red => red = body; red
	    case Blue => blue = body; blue
	    case _ => none = body; none
	} 
    def update(which : Sided)(body : T => T):T = update(which.currentSide)(body)
   	def update(which : Value)(body : T=> T):T = which match  {
		case Red => red = body(red); red
	    case Blue => blue  = body(blue); blue
	    case _ => none = body(none); none 
	} 
    def get(which : Sided) : T = get(which.currentSide)
	def get(which : Value) : T= which match  {
		case Red => red
	    case Blue => blue
	    case _ => none      
	}
 
  	def of(which : Sided) = new side(which.currentSide)
  	def of(which : Army.Value) = new side(which)
  	class side(which : Value) {
  	  def side = get(which)
  	  def side_=(n : => T) = set(which)(n)
  	  def update(body : T=>T): T = SidesVar.this.update(which)(body)
  	}
    
 
// 	def army (currentSide : Value)(x : Any) = {
//	  x match {
//	    case s : Function0[T] => set(currentSide){s.apply}
//	    case u : (T=>T) => update(currentSide)(u)
//	    case s : T => set(currentSide){s}
//	    case g : Unit => get(currentSide)
//	  } 
//	}
//  
// 	def apply (sided : Sided) : Function1[Any, T] = new Function1[Any, T]{
// 	  
//	  override def apply(x : Any) : T = x match {
//	    case s : Function0[T] => set(sided.currentSide){s.apply}
//	    case u : (T=>T) => update(sided.currentSide)(u)
//	    case s : T => set(sided.currentSide){s}
//	    case g : Unit => get(sided.currentSide)
//	  } 
//	}
  
  }
  
  
  class SidesVal[T](x : => T) {
	val blue : T = x 
	val red  : T = x
	val none : T = x

	def get(which : Sided) : T = get(which.currentSide)
	def get(which : Value) : T= which match  {
		case Red => red
	    case Blue => blue
	    case _ => none
	}
    def of(which : Army.Value) = new side(which)
  	def of(which : Sided) = new side(which.currentSide)
  	class side(which : Value) {
  	  def side = get(which)
//  	  def side_=(n : T) = set(whic)
  	}
  }
}

trait Sided {
	val currentSide : Army.Value= Army.None
	def other = currentSide match {
	  case Army.Red => Army.Blue
	  case Army.Blue => Army.Red
	  case Army.None => Army.None 
	}
//	def get[T](v : Army.SidesVal[T]) = v.get(currentSide)
//	def get[T](v : Army.SidesVar[T]) = v.get(currentSide)
//	def set[T](v : Army.SidesVar[T])(f: =>T) = v.set(currentSide)(f)
//	def update[T](v : Army.SidesVar[T])(f: T=>T) = v.update(currentSide)(f)
//	
//	def side[T] (v : Army.SidesVal[T]) : DoVal[T] = new DoVal[T](v, currentSide)
//	class DoVal[T](v  : Army.SidesVal[T], s : Army.Value){
//	  def get : T = {v.get(s)}
//	}
// 
//	def side[T] (v : Army.SidesVar[T]) : DoVar[T] = new DoVar[T](v, currentSide)
//	class DoVar[T](v  : Army.SidesVar[T], s : Army.Value){
//	  def get() : T = {v.get(s)}
//	  def set(b : =>T) = v.set(s)(b)
//	  def update(b:T=>T) = v.update(s)(b)
//	  def is = get
//	  def is_=(b : => T) = set(b)
//	}
//	
}