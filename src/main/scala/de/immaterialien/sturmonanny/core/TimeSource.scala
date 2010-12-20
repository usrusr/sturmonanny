package de.immaterialien.sturmonanny.core

trait TimeSource {
	def currentTimeMillis:Long
	final def now = currentTimeMillis
}

class TimeSourceImpl extends TimeSource{
	def currentTimeMillis = System.currentTimeMillis
}