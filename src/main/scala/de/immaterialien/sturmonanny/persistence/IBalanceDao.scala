package de.immaterialien.sturmonanny.persistence
import de.immaterialien.sturmonanny._

trait IBalanceDao {
	def load(pilot:String):Option[IBalanceDao.BalanceRB]
	def store(pilot:String, balanceRed:Option[Double], balanceBlue:Option[Double])
	
	/**
	 * @param props 
	 * @param info dont store!
	 * @param error dont store!
	 */
	def open( props:Map[String, String], info:String=>Unit, error:String=>Unit) 
	def close(info:String=>Unit, error:String=>Unit)
}

object IBalanceDao {
	case class BalanceRB(red:Double, blue:Double)
}
/**
 * dummy store
 */
class InMemoryBackend extends IBalanceDao { 
	def load(pilot:String):Option[IBalanceDao.BalanceRB] = None
	def store(pilot:String, balanceRed:Option[Double], balanceBlue:Option[Double]){
println(" dummy storing "+pilot+" -> r:"+balanceRed+" b:"+balanceBlue);		
	}
	def open(props:Map[String, String], info:String=>Unit, error:String=>Unit){}
	def close(info:String=>Unit, error:String=>Unit){}
}