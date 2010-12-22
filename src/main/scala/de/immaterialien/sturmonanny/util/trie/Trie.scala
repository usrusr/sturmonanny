package de.immaterialien.sturmonanny.util.trie

import java.util.{Comparator, Arrays}


  object Trie{
    /**
     * a stupid little default comparator using natural comparable ordering or, for noncomparables 
     * natural toString ordering
     */
    protected object toStringComparator_ extends Comparator[AnyRef] {
      def compare(left, right) = {
        if(left.isInstanceOf[Comparable[_]]) left.asInstanceOf[Comparable[AnyRef]].compareTo(right) 
        else (""+left) compareTo (""+right)
      }
    }
    protected def toStringComparator[C] = toStringComparator_.asInstanceOf[Comparator[C]]
    
    protected def seqCompare[C](pl:Iterable[C], pr:Iterable[C], comp:Comparator[C])={
      var lh = pl.headOption
        var rh = pr.headOption
        var lt = if(pl.isEmpty) List() else pl.tail 
        var rt = if(pr.isEmpty) List() else pr.tail
        
        var res = 0
        
        while(lh.isDefined && rh.isDefined && res==0){
          res = comp.compare(lh.get, rh.get)
          
          lh = lt.headOption
          rh = rt.headOption
          lt = if(lt.isEmpty) List() else lt.tail
          rt = if(rt.isEmpty) List() else rt.tail
        }
        
        if(res == 0){
          if(rh.isDefined) res = -1
          else if(lh.isDefined) res = +1
        }
        res
    }
    /**
     * while it _does_ compare prefixes of Seqs this comparator is named after the fact that it compares 
     * the prefixes of WithPrefix instances
     * 
     * <p> the actual sequence comparison is in a static method because it is also used by TrieNode for 
     * Seq-implementation independent equality checks
     */
    protected class PrefixComparator[C](comp : Comparator[C]) extends Comparator[WithPrefix[C]] {
      def compare(left, right)={
        val pl = left.prefix
        val pr = right.prefix

        seqCompare(pl, pr, comp)
      }
    }
  
    protected trait WithPrefix[C] {
      def prefix : Iterable[C]
    }

    protected[trie] class TrieNode[C,V]  (prefix_ : Seq[C], private val value:Option[V], rests_ :Array[TrieNode[C,V]]) extends Trie.WithPrefix[C] {
      def prefix:Seq[C] = prefix_
      def rests:Array[TrieNode[C,V]] = rests_
  
      
      private def returnDeeper(deeper:Option[(Seq[C], V)]):Option[(Seq[C], V)]=deeper.map{tailAndValue=>
        val concatenated : Seq[C] = prefix.toList ::: tailAndValue._1.toList
        (concatenated, tailAndValue._2)
      }
      def find(query:Iterable[C], longest:Boolean, comp : Comparator[C] ):Option[(Seq[C], V)]={
        if( value.isDefined && (query.headOption.isEmpty || ! longest)) {
          Some(prefix, value.get)
        }else if( value.isDefined && rests.isEmpty) {
          Some(prefix, value.get)
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
            if(invInsertion >= rests.length){
              None
            }else{
              val nextBigger = rests(invInsertion)
              val nextLen = nextBigger.prefix.length
              val nextBiggerPrefix = nextBigger.prefix.toSeq
              val queryPrefix = query.take(nextLen).toSeq
              if(seqCompare(nextBiggerPrefix, queryPrefix, comp)==0){
                returnDeeper(nextBigger.find(query.drop(nextLen), longest, comp))
              }else{
                None
              }
            }
          }
        }
      }
      def binarySearch(query:Iterable[C], comp : Comparator[C]):Int={
        object prefixHolder extends Trie.WithPrefix[C]{
          def prefix = query
        }
        binarySearch(prefixHolder, comp)
      }
      def binarySearch(prefixHolder:Trie.WithPrefix[C], comp : Comparator[C]):Int={
        val arr = rests.asInstanceOf[Array[Object]]
        val ret = Arrays.binarySearch(arr, 
            prefixHolder, 
            new Trie.PrefixComparator[C](comp).asInstanceOf[Comparator[Object]]
        )
        
        ret
      }

      def foreach[U](head:Seq[C], f: ((Seq[C], V)) => U): Unit = {
        lazy val cur = head++prefix
        for(v<-value){
          f(cur, v)
        }
        for(r<-rests){
          r.foreach(cur, f)
        }
      }
      
      override def toString = {
        val sb = new StringBuilder
        buildString(sb, 0)
        sb.toString
      }
      def buildString(sb:StringBuilder, indent:Int){
        sb.append("\n")
        for(i<-0 to indent) sb.append(" ")
        val string = ""+prefix
        sb.append(string) 
        if(value.isDefined) sb.append(" [").append(value.get).append("]")
        else sb.append(" ( )")
        for(c<-rests) c.buildString(sb, indent+string.length)
      }
      def mergeRests(comp : Comparator[C], one:Array[TrieNode[C,V]]* ):Array[TrieNode[C,V]]={
        val filled = one.filter(_.length > 0).toBuffer
        filled.size match{
          case 0 =>Array()
          case 1 =>filled.head
          case _ => {
            val ret = Array.concat(filled:_*)
            //Arrays.sort(ret.asInstanceOf[Array[AnyRef]], comp.asInstanceOf[Comparator[AnyRef]])
            Arrays.sort(ret.asInstanceOf[Array[AnyRef]], new Trie.PrefixComparator[C](comp).asInstanceOf[Comparator[AnyRef]])
            ret
          }
        }
      }
      
      
      
      def add(toAdd:TrieNode[C,V], comp : Comparator[C]):TrieNode[C,V]={
        val index = binarySearch(toAdd, comp )
        if(index > -1){
          val oldSub = rests(index)
          val newRests = mergeRests(comp, rests, toAdd.rests)
          val newSub :TrieNode[C,V] = new TrieNode[C,V](toAdd.prefix, toAdd.value , newRests)
          val newArray = rests.clone
          newArray.update(index, newSub)
          new TrieNode[C,V](prefix_, value, newArray)
        }else {
          val insertion = (index * -1)-1
          if( // identify "insert" case
              (insertion==0 && rests.length==0)
              || (insertion==0 && insertion < rests.length 
                  && ! rests(0).prefix.startsWith(toAdd.prefix.take(1))
                  )
              || (insertion >= rests.length && insertion>0
                  && ! rests(insertion-1).prefix.startsWith(toAdd.prefix.take(1))
                  )

              || ! (
                  insertion >= rests.length 
                  || (insertion>0 && rests(insertion-1).prefix.startsWith(toAdd.prefix.take(1)))
                  || (rests(insertion).prefix.startsWith(toAdd.prefix.take(1)))
              )
            ){
            val newSub :TrieNode[C,V] = new TrieNode[C,V](toAdd.prefix.toBuffer, toAdd.value, Array())
            
            val before = rests.take(insertion)
            val mid = Array(newSub)
            val after = rests.takeRight(rests.size-insertion)
            
            val newArray = Array.concat[TrieNode[C,V]](before, mid, after)
            new TrieNode[C,V](prefix_, value, newArray)
          }else{
            val editIndex = if(insertion>0 && rests(insertion-1).prefix.startsWith(toAdd.prefix.take(1))){
              insertion-1
            }else{
              insertion
            }
            
            val editChild = rests(editIndex)
            val addLen = toAdd.prefix.length
            val editLen = prefix.length

            val (longer, longerVal, longerRest, shorter, shorterVal, shorterRest)
              :(Seq[C],Option[V], Array[TrieNode[C,V]],Seq[C],Option[V], Array[TrieNode[C,V]]) 
              = if(addLen>editLen) 
                  (toAdd.prefix, toAdd.value, toAdd.rests, editChild.prefix, editChild.value, editChild.rests) 
                else 
                  (editChild.prefix, editChild.value, editChild.rests, toAdd.prefix, toAdd.value, toAdd.rests)
            
            val edited = if(longer.startsWith(shorter)){
              val newLonger = new TrieNode[C,V](longer.drop(shorter.length).toBuffer, longerVal, longerRest)
              //val newShorter = new TrieNode[C,V](shorter, shorterVal, mergeRests(comp, shorterRest, Array(newLonger)))
              //newShorter
              val newShorter = new TrieNode[C,V](shorter, shorterVal, shorterRest)
              val merged = newShorter.add(newLonger, comp)
              merged
              //val newShorter = shorter.add(newLonger, comp)
              
              
            }else{
              val zipped = shorter.zip(longer)
              val prefix = zipped.takeWhile((xy) => xy._1==xy._2).map(_ _1).toBuffer
              val newLonger = new TrieNode[C,V](longer.drop(prefix.length).toBuffer, longerVal, longerRest)
              val newShorter = new TrieNode[C,V](shorter.drop(prefix.length).toBuffer, shorterVal, shorterRest)
              val newBranching = new TrieNode[C,V](prefix, None, mergeRests(comp, Array(newLonger), Array(newShorter)))
              newBranching
            }
            val newArray = rests.clone
            newArray.update(editIndex, edited)
            new TrieNode[C,V](prefix, value, newArray)
          }
        }
      }
    }
  }
  
  /**
   * An immutable trie implementation for Seq[C] featuring two special get variants longest and shortest 
   * that can identify prefixes of a possibly infinite query sequence (think parsers!) 
   * 
   * remove creates copies without cleanup, so an occasional rebuild (via foreach) might be good after excessive removal
   * 
   * @param comp: you may supply your own element comparator (otherwise defaulting to per-element switched natural/toString fallback which is 
   * slow, so you might want to supply something like Ordering.natural yourself for C<Comparable[C]   
   */
  class Trie[C, V] private (size:Int, protected val comp :Comparator[C], private val root:Trie.TrieNode[C,V]) extends Traversable[(Seq[C], V)]{
    def this(comp :Comparator[C]=Trie.toStringComparator[C])=this(0, comp, new Trie.TrieNode[C,V](List(), None, Array()))
    def add(k:Seq[C], v:V):Trie[C,V]={
      val increment = if(get(k).isDefined) 0 else 1
      new Trie(size+increment, comp, root.add(new Trie.TrieNode[C,V](k, Some(v), Array()), comp))
    }

    def remove(k:Seq[C]):Trie[C,V]={
      if(get(k).isEmpty) this
      else new Trie(size-1, comp, root.add(new Trie.TrieNode[C,V](k, None, Array()), comp))
    }
    
    def get(key:Seq[C]):Option[V]={
      root.find(key, true, comp) match {
        case Some((found, v)) if(Trie.seqCompare(found,key, comp)==0) => Some(v)
        case _ => None
      }
    }
    def longest(key:Iterable[C]):Option[(Seq[C], V)]=root.find(key, true, comp) 
    def shortest(key:Iterable[C]):Option[(Seq[C], V)]=root.find(key, false, comp)


    def foreach[U](f: ((Seq[C], V)) => U): Unit = {
        root.foreach(List(), f)
    }

    override def toString = {
      val sb = new StringBuilder
      root.buildString(sb, 0)
      sb.toString
    }
  }
