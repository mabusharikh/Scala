package forcomp

import forcomp.Anagrams.sentenceAnagrams

/**
  * Created by Mohammed on 08/02/2017.
  */
object Main extends App{

  val t0 = System.nanoTime()
  val s1 = List("I", "love", "you")
  val s2 = List("Linux", "rulez")
  println(sentenceAnagrams(s2))
  println("Took " + ((System.nanoTime() - t0)/1000000 ) + "ms");
}
