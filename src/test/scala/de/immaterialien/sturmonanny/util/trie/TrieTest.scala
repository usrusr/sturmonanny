package de.immaterialien.sturmonanny.util.trie

  object TrieTest {
    def main(i:Array[String]){
      var t = new Trie[Char, Int]() 
      t = t.add("hallo", 10)
      t = t.add("hai", 2)
      t = t.add("welcome", 3)
      t = t.add("tach", 4)
      t = t.add("tachchen", 44)
      t = t.add("bonjour", 5)
      println("---built---"+t)
      
      println(t.longest("halloele")) 
      
      println(t.get("hai"))
      println(t.get("bye")) 
      println(t.get("tachchensch�n")) 
      println(t.shortest("tachchensch�n")) 
      println(t.longest("tachchensch�n")) 
    }
  }
