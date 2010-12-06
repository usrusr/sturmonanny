package de.immaterialien.sturmonanny.util

import java.util.{Comparator, Arrays}

object Trie {
	private object toStringComparator_ extends Comparator[AnyRef] {
		def compare(left, right)=(""+left) compareTo (""+right)
	}
	private def toStringComparator[T] = toStringComparator_.asInstanceOf[Comparator[T]]
	private class PrefixComparator[T](comp : Comparator[Seq[T]]) extends Comparator[WithPrefix[T]] {
		def compare(left, right)=comp.compare(left.prefix, right.prefix)
	}

	private val test = new Trie[Char, Boolean]("hatschi", None, Array(), toStringComparator)
	private trait WithPrefix[T] {
		def prefix : Seq[T]
	}
}
class Trie[T, V]  (prefix_ : Seq[T], private val value:Option[V], rests_ :Array[Trie[T, V]], comp_ :Comparator[Seq[T]]) extends Trie.WithPrefix[T] {
	// def this(init:Seq[T]=List(), v:V=null, comparator:Comparator[T]=Trie.toStringComparator) = this(init, Some(v), Array(), comparator)
	def prefix:Seq[T] = prefix_
	def rests:Array[Trie[T, V]] = rests_
	def comp = comp_
	def add(rest:Seq[T], inValue:V):Trie[T,V]={
		object prefixHolder extends Trie.WithPrefix[T]{
			def prefix = rest
		}
	
		val index = Arrays.binarySearch(rests.asInstanceOf[Array[Object]], 
				prefixHolder, 
				new Trie.PrefixComparator[T](comp).asInstanceOf[Comparator[Object]]
		)
		if(index > -1){
			val oldSub = rests(index)
			val newSub :Trie[T,V] = new Trie(rest, Some(inValue), oldSub.rests, oldSub.comp)
			val newArray = rests.clone
			newArray.update(index, newSub)
			new Trie[T,V](prefix_, value, newArray, comp)
		}else {
			val insertion = (index * -1)-1
			val before = rests(insertion-1)
			if( // identify "insert" case
				insertion==0 
				|| {
					
					val restLen = rest.length
					if(before.prefix.length <= restLen) true else {
						val commonLen = before.prefix.take(restLen)
						if(commonLen == rest) {
							// before is a prefix of our new rest
							false 
						} else {
							true
						}
					}
				}){
				// insert
				val newSub :Trie[T,V] = new Trie(rest, Some(inValue), Array(), comp)
				val newArray = Array.concat[Trie[T, V]](rests.take(insertion), Array(newSub), rests.takeRight(rests.size-insertion))
				new Trie[T,V](prefix_, value, newArray, comp)
			}else{
				// go deeper
				val updateAt = insertion-1
				val newSub = before.add(rest.drop(before.prefix.length), inValue)
				val newArray = rests.clone
				newArray.update(insertion-1, newSub)
				new Trie[T,V](prefix_, value, newArray, comp)				
			}
		}
	}


} 