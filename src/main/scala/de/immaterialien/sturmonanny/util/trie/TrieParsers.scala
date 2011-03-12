package de.immaterialien.sturmonanny.util.trie

import scala.util.parsing.combinator._
import scala.collection.mutable

trait TrieParsers extends Parsers{
	private class InputWrapper(in:Input) extends Iterable[Elem] {
		override def headOption = if(in.atEnd) None else Some(in.first)
		override def iterator : Iterator[Elem] = new Iterator[Elem] {
			//var cur = in.drop(in.offset)
			var cur = in
//println("next "+cur.first+ "in.offset:"+in.offset)				
			override def hasNext = cur.atEnd
			override def next = {
				val ret = cur.first
				cur = cur.drop(1)
//println("next "+ret+ "")				
				ret
			}
		}
		override def take(howmany:Int)={
//			val ret = super.take(howmany)


			var i=0
			var buf = new mutable.ArrayBuffer[Elem](howmany)
			var inCur = in
			while(i<howmany && ! in.atEnd){
				buf += inCur.first
				inCur = inCur.rest
				i=i+1
			}
			
//println("taking "+howmany+" :"+buf)			
			buf
//			ret
		}
		override def drop(howmany:Int)={
			val ret = new InputWrapper(in.drop(howmany))
//println("dropping "+howmany+" :")			
			ret
		}
	}
	
	class TrieMapParser[V] extends Parser[V] {
		var trie = new Trie[Elem, V]()
		def add(addee:Seq[Elem], v:V)={
			trie = trie.add(addee, v)
//println("----------\nadded:"+addee.mkString("")+"\n"+trie)			
		}
		def remove(addee:Seq[Elem])={
			trie = trie.remove(addee)
		}		

		//override def apply(in:Reader)
		override def apply(in: Input):ParseResult[V]={
			val inWrap = new InputWrapper(in)
			val found = trie.longest(inWrap)
			if(found.isDefined){
				val (foundSeq, foundVal) = found.get
//println("found "+foundSeq)				
				Success(foundVal, in.drop(foundSeq.length))
			}else{
				Failure("not contained in trie", in)
			}
		}
	}
	
	class TrieParser extends Parser[Seq[Elem]] {
		var trie = new Trie[Elem, Boolean]()
		def add(addee:Seq[Elem])={
			trie = trie.add(addee, true)
		}
		def remove(addee:Seq[Elem])={
			trie = trie.remove(addee)
		}		
		def clear = trie = new Trie[Elem, Boolean]()
		def contains(what:Seq[Elem]) = trie.get(what).isDefined

		//override def apply(in:Reader)
		override def apply(in: Input):ParseResult[Seq[Elem]]={
			val inWrap = new InputWrapper(in)
			val found = trie.longest(inWrap)
			val ret  = if(found.isDefined){
				val foundSeq = found.get._1
				Success(foundSeq, in.drop(foundSeq.length))
			}else{
				Failure("not contained in trie", in)
			}
//println("pares "+ret+ " "+trie)			
			ret
		}
		/**
		 * convert the Parser[Seq[Elem]] into something more useful<p>
		 * the most conventional use would be new TrieParser().map{seq => new String(seq.toArray)}
		 * 
		 * @param func
		 * @return
		 */
		override def map[T](func:Seq[Elem]=>T)=new MappedParser(this, func)
	}
	
	class MappedParser[T](tp:TrieParser, func:Seq[Elem]=>T) extends Parser[T] {
//		val tp = new TrieParser
		def contains(what:Seq[Elem])=tp.contains(what)
		def add(addee:Seq[Elem])=tp.add(addee)
		def remove(addee:Seq[Elem])=tp.remove(addee)
		def clear = tp.clear
		def apply(in:Input):ParseResult[T]={
			tp.apply(in) match {
				case Success(seq, rest) => {
					val list = func(seq)
					Success(list, rest)
				}
				case Failure(msg, rest) => Failure(msg, rest)
				case Error (msg, rest) => Error(msg, rest)
			}
		}
	}
	
}