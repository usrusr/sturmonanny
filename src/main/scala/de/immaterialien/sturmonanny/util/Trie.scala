package de.immaterialien.sturmonanny.util

import java.util.{Comparator, Arrays}

package trie {
  object Trie{
    protected object toStringComparator_ extends Comparator[AnyRef] {
      def compare(left, right)=(""+left) compareTo (""+right)
    }
    protected def toStringComparator[T] = toStringComparator_.asInstanceOf[Comparator[T]]
    protected class PrefixComparator[T](comp : Comparator[Seq[T]]) extends Comparator[WithPrefix[T]] {
      def compare(left, right)=comp.compare(left.prefix, right.prefix)
    }
  
    protected trait WithPrefix[T] {
      def prefix : Seq[T]
    }

    protected[trie] class TrieNode[T,V]  (prefix_ : Seq[T], private val value:Option[V], rests_ :Array[TrieNode[T,V]]) extends Trie.WithPrefix[T] {
      // def this(init:Seq[T]=List(), v:V=null, comparator:Comparator[T]=TrieNode[T,V].toStringComparator) = this(init, Some(v), Array(), comparator)
      def prefix:Seq[T] = prefix_
      def rests:Array[TrieNode[T,V]] = rests_
  
      
      private def returnDeeper(deeper:Option[(Seq[T], V)]):Option[(Seq[T], V)]=deeper.map{tailAndValue=>
        val concatenated : Seq[T] = prefix.toList ::: tailAndValue._1.toList
        (concatenated, tailAndValue._2)
      }
      def find(query:Seq[T], longest:Boolean, comp : Comparator[Seq[T]] ):Option[(Seq[T], V)]={
        if( value.isDefined && (query.headOption.isEmpty || ! longest)) {
          Some(List(), value.get)
        }else{
          val insertion = binarySearch(query.take(1), comp)
          if(insertion > -1) {
            returnDeeper(rests(insertion).find(query.drop(1), longest, comp))
          } else {
            val invInsertion = (-1 * insertion)-1
            if(insertion == rests.length) {
              // bigger
              return None
            }
            val nextBigger = rests(invInsertion+1)
            val nextLen = nextBigger.prefix.length
            if(query.take(nextLen) == nextBigger.prefix){
              returnDeeper(nextBigger.find(query.drop(nextLen), longest, comp))
            }else{
              None
            }
          }
          None
        }
      }
      def binarySearch(query:Seq[T], comp : Comparator[Seq[T]]):Int={
        object prefixHolder extends Trie.WithPrefix[T]{
          def prefix = query
        }
        binarySearch(prefixHolder, comp)
      }
      def binarySearch(prefixHolder:Trie.WithPrefix[T], comp : Comparator[Seq[T]]):Int={
        Arrays.binarySearch(rests.asInstanceOf[Array[Object]], 
            prefixHolder, 
            new Trie.PrefixComparator[T](comp).asInstanceOf[Comparator[Object]]
        )
      }

      override def toString = prefix + ":"+value+" *"+rests.size  
      def buildString(sb:StringBuilder, indent:Int){
        sb.append("\n")
        for(i<-0 to indent) sb.append(" ")
        val string = ""+prefix
        sb.append(string) 
        if(value.isDefined) sb.append(" [").append(value.get).append("]")
        else sb.append(" ( )")
        for(c<-rests) c.buildString(sb, indent+string.length)
      }
      def mergeRests(comp : Comparator[Seq[T]], one:Array[TrieNode[T,V]]* ):Array[TrieNode[T,V]]={
        val filled = one.filter(_.length > 0).toBuffer
        filled.size match{
          case 0 =>Array()
          case 1 =>filled.head
          case _ => {
            val ret = Array.concat(filled:_*)
            //Arrays.sort(ret.asInstanceOf[Array[AnyRef]], comp.asInstanceOf[Comparator[AnyRef]])
            Arrays.sort(ret.asInstanceOf[Array[AnyRef]], new Trie.PrefixComparator[T](comp).asInstanceOf[Comparator[AnyRef]])
            ret
          }
        }
      }
      
      
      
      def add(toAdd:TrieNode[T,V], comp : Comparator[Seq[T]]):TrieNode[T,V]={
        val index = binarySearch(toAdd, comp )
        if(index > -1){
          val oldSub = rests(index)
          val newRests = mergeRests(comp, rests, toAdd.rests)
          val newSub :TrieNode[T,V] = new TrieNode[T,V](toAdd.prefix, toAdd.value , newRests)
          val newArray = rests.clone
          newArray.update(index, newSub)
          new TrieNode[T,V](prefix_, value, newArray)
        }else {
          val insertion = (index * -1)-1
          if( // identify "insert" case
              insertion==0 
              || insertion == rests.length
              || ! (
                   rests(insertion-1).prefix.startsWith(toAdd.prefix.take(1))
                || rests(insertion).prefix.startsWith(toAdd.prefix.take(1))
              )
            ){
            val newSub :TrieNode[T,V] = new TrieNode[T,V](toAdd.prefix.toBuffer, toAdd.value, Array())
            val newArray = Array.concat[TrieNode[T,V]](rests.take(insertion), Array(newSub), rests.takeRight(rests.size-insertion))
            new TrieNode[T,V](prefix_, value, newArray)
          }else{
            val editIndex = if(rests(insertion-1).prefix.startsWith(toAdd.prefix.take(1))){
              insertion-1
            }else{
              insertion
            }
            
            val editChild = rests(editIndex)
            val addLen = toAdd.prefix.length
            val editLen = prefix.length

            val (longer, longerVal, longerRest, shorter, shorterVal, shorterRest)
              :(Seq[T],Option[V], Array[TrieNode[T,V]],Seq[T],Option[V], Array[TrieNode[T,V]]) 
              = if(addLen>editLen) 
                  (toAdd.prefix, toAdd.value, toAdd.rests, editChild.prefix, editChild.value, editChild.rests) 
                else 
                  (editChild.prefix, editChild.value, editChild.rests, toAdd.prefix, toAdd.value, toAdd.rests)
            
            val edited = if(longer.startsWith(shorter)){
              val newLonger = new TrieNode[T,V](longer.drop(shorter.length).toBuffer, longerVal, longerRest)
              val newShorter = new TrieNode[T,V](shorter, shorterVal, mergeRests(comp, shorterRest, Array(newLonger)))
              
              newShorter
            }else{
              val zipped = shorter.zip(longer)
              val prefix = zipped.takeWhile((xy) => xy._1==xy._2).map(_ _1).toBuffer
              val newLonger = new TrieNode[T,V](longer.drop(prefix.length).toBuffer, longerVal, longerRest)
              val newShorter = new TrieNode[T,V](shorter.drop(prefix.length).toBuffer, shorterVal, shorterRest)
              val newBranching = new TrieNode[T,V](prefix, None, mergeRests(comp, Array(newLonger), Array(newShorter)))
              newBranching
            }
            val newArray = rests.clone
            newArray.update(editIndex, edited)
            new TrieNode[T,V](prefix, value, newArray)
          }
        }
      }
    }
  }
  class Trie[T, V] private (protected val comp :Comparator[Seq[T]], private val root:Trie.TrieNode[T,V]){
    def this(comp :Comparator[Seq[T]]=Trie.toStringComparator[Seq[T]])=this(comp, new Trie.TrieNode[T,V](List(), None, Array()))
    def add(k:Seq[T], v:V):Trie[T,V]={
      new Trie(comp, root.add(new Trie.TrieNode[T,V](k, Some(v), Array()), comp))
    }
    def get(key:Seq[T]):Option[V]={
      root.find(key, true, comp) match {
        case Some((key, v)) => Some(v)
        case _ => None
      }
    }
    def longest(key:Seq[T]):Option[(Seq[T], V)]=root.find(key, true, comp) 
    def shortest(key:Seq[T]):Option[(Seq[T], V)]=root.find(key, false, comp)
    
    override def toString = {
      val sb = new StringBuilder
      root.buildString(sb, 0)
      sb.toString
    }
  }
} 