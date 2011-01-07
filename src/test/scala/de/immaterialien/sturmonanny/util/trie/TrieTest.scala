package de.immaterialien.sturmonanny.util.trie
	import org.junit.Test
	import org.junit.Assert._

  class TrieTest {
		@Test
    def main{
      var t = new Trie[Char, Int]() 
      t = t.add("hallo", 10)
      t = t.add("hey", 5)
      t = t.add("hai", 2)
      t = t.add("welcome", 3)
      t = t.add("tach", 4)
      t = t.add("tachchen", 44)
      t = t.add("bonjour", 55)
      println("---built---"+t)

      
      
			assertEquals(10, t.longest("halloele").get._2) 
			  
			assertEquals(2, t.get("hai").get)
			assertTrue(t.get("bye").isEmpty) 
			assertTrue(t.get("tachchenschen").isEmpty) 
			assertEquals(4,t.shortest("tachchenschen").get._2) 
			assertEquals(44,t.longest("tachchenschen").get._2) 
			
			//readd hey
      t = t.add("hey", 99)      
      println("---readded hey---"+t)

      
      for(n<-t if(n._1 == "hey".toSeq)){
      	println("found hey")
      }
      for(n<-t if(n._1 == "heyey".toSeq)){
      	println("found heyey")
      }
      
      val fivesum = t.filter(_._2 == 5).map(_ _2).sum
      assertEquals("the only 5 value should have been removed by adding (hey,99)", 0, fivesum)
      val tensum = t.filter(_._2 == 10).map(_ _2).sum
      assertEquals("the only 10 value should be there only once after adding (hey,99)", 10, tensum)
		}
		
		@Test
		def iiiJG27{
			var trie = new Trie[Char, Int]() 
			trie = trie.add("III/JG27_Hias",1)
			trie = trie.add("III/JG27*Angus",1)
			trie = trie.add("=Olo=",1)
			trie = trie.add("MAD-MM",1)
			trie = trie.add("I/JG11_Kalle",1)
			trie = trie.add("Rudi.",1)
			trie = trie.add("Peter5on",1)
			trie = trie.add("Gimmy",1)
			trie = trie.add("LLv24_Max",1)
			trie = trie.add("III/JG27_Flix",1)
			trie = trie.add("3BAG_Titarenko",1)
			trie = trie.add("JG333_Hasenfuss",1)
			trie = trie.add("OSLA",1)
			trie = trie.add("wurzel",1)
			trie = trie.add("Blue_MiK",1)
			trie = trie.add("<*_*>",1)
			println("====1====="+trie)
			trie = trie.add("III/JG27_Tbag",1)
			println("====2====="+trie)
			trie = trie.add("III/JG27_Flix",1)
			println("====3====="+trie)
			trie = trie.add("III/JG27_Tbag",1)
			println("====4====="+trie)
			trie = trie.add("III/JG27_Flix",1)
			trie = trie.add("III/JG27_Tbag",1)
			trie = trie.add("III/JG27_Flix",1)
			trie = trie.add("III/JG27_Tbag",1)
			
			val flixcount = trie.filter(pair => new String(pair._1.toArray) == "III/JG27_Flix").map(_._2).sum
			assertTrue(1 == flixcount)
			
			println("after III/JG27_ :"+trie)
		}
		
		@Test
    def parser{
			
			object ps extends TrieParsers with scala.util.parsing.combinator.RegexParsers{
//				type Elem = Char
				
				lazy val threeNumsSum = (""~> nums ~""~ nums ~""~ nums) ^^ {
					case n1 ~ _ ~ n2 ~ _ ~ n3 => n1 + n2 + n3
				}
				
				lazy val sums = repsep(threeNumsSum, ",")
				
				lazy val single = nums <~ ""
				lazy val nums = {
					val p = new TrieMapParser[Int]()
					p.add("one",1)
					p.add("two",2)
					p.add("three",3)
					p 
				}
			}
			
			println("parser: "+ ps.nums.trie)

			val res0 = ps.parseAll(ps.nums, "one")
      println("one: "+ res0)
      assertEquals(1,res0.get)
 
			
      val res1 = ps.parseAll(ps.threeNumsSum, "one  two three")
      println("one  two three: "+ res1)
      assertEquals(6,res1.get)

      val res2 = ps.parseAll(ps.threeNumsSum, "one  twotwo")
      println("one  twotwo: "+ res2)
      assertEquals(5,res2.get)
      
      val res3 = ps.parseAll(ps.sums, "one  twotwo, threethreeone, three two 	one")
      println("one  twotwo, threethreeone, three two 	one: "+ res3)
      assertEquals(List(5,7,6),res3.get)
    }

  }
