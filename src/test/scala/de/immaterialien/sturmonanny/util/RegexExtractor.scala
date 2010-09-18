package de.immaterialien.sturmonanny.util

object RegexExtractor {
	def main(args: Array[String]) {
		val rx = """(?i-)([abc])(\d+)""".r
		
		"A12" match {
			case rx("A", "12") => println("a-twelve") 
			case _ => println("other") 
		}
	}
}