package de.immaterialien.sturmonanny.util

trait TimeSource {
	
	def currentTimeMillis:Long
	final def now = currentTimeMillis
}
trait TimeHolder {
//println("TimeHolder hello")	
	def time : TimeSource
}

class TimeSourceImpl extends TimeSource{
	def currentTimeMillis = System.currentTimeMillis 
}