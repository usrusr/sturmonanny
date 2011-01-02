package de.immaterialien.sturmonanny.util

trait TimeSource {
	def currentTimeMillis:Long
	final def now = currentTimeMillis
}

class TimeSourceImpl extends TimeSource{
	def currentTimeMillis = System.currentTimeMillis 
}