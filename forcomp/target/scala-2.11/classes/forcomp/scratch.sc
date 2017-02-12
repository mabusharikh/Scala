import java.io.Serializable

import forcomp.{Anagrams, loadDictionary}

/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  *  how often the character appears.
  *  This list is sorted alphabetically w.r.t. to the character in each pair.
  *  All characters in the occurrence list are lowercase.
  *
  *  Any list of pairs of lowercase characters and their frequency which is not sorted
  *  is **not** an occurrence list.
  *
  *  Note: If the frequency of some character is zero, then that character should not be
  *  in the list.
  */
type Occurrences = List[(Char, Int)]

/** The dictionary is simply a sequence of words.
  *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
  */
//val dictionary: List[Word] = loadDictionary

/** Converts the word into its character occurrence list.
  *
  *  Note: the uppercase and lowercase version of the character are treated as the
  *  same character, and are represented as a lowercase character in the occurrence list.
  *
  *  Note: you must use `groupBy` to implement this method!
  */
//def wordOccurrences(w: Word) = ((w.toLowerCase() groupBy(x=>x) map  (c=>(c._1, c._2.length()))) toList).sortBy(_._1)
//
//def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)
//sentenceOccurrences(List("Mohammed","is","Monkey"))
//
//val dictionary =loadDictionary
//
//lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences
//
//
//def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences get wordOccurrences(word) getOrElse Nil
//

//def comb1(occ: Occurrences) = for(i <- 1 to occ.length) yield occ combinations i toList

//
//def comb3(occ: Occurrences) : List[Occurrences]= {
//  for {
//    idx <- 0 until  occ.length
//    elem <- occ(idx)
//    i <- 1 to 2
//  } yield occ updated(idx, (elem, i))
//}


  //def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
//  case Nil => List()
//  case x::xs=>if(x._2 == 0) combinations(xs) else comb1(x :: flatMap combinations(xs)
//}


//val test=List(('a',2), ('b',2))
//
//def denormalize(occurrences :Occurrences) :Occurrences = for {
//    elem<-occurrences
//    j<-1 to elem._2
//  } yield (elem._1, 1)
//
//def combinations(occurences: Occurrences) :List[Occurrences] = {
//    val denomOcc = denormalize(occurences)
//  var denomCombs = for {
//    i <- 0 to denomOcc.length
//    combs <- denomOcc combinations i
//  } yield combs
//
//  (for (comb <- denomCombs) yield (comb groupBy (_._1)  map (x=>(x._1, x._2.length))  toList) sortBy(_._1)) toList
//}

//combinations(test)
//combinations(test)

//test.toSet.subsets.toList
//test(0)
//comb1(test)
//comb3(test)
//test combinations(1) toList


def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  var commonX = x filter (a => y exists (z => a._1 == z._1))
  val adjusted = (for ((m,n)<-(commonX zip y)) yield  (m._1, m._2 - n._2)) filter (_._2 > 0)
  var rest = x filter (a => y forall (z => a._1 != z._1))
  //mappedY.foldLeft(mappedX) ((a,p)=> a)
  adjusted ::: rest
}


def subtract2(x:Occurrences, y:Occurrences) : Occurrences= {
  val mappedX = x toMap
  val mappedY = y toMap

  (mappedY.foldLeft(mappedX) ((a,p)=> a+((p._1, a(p._1) - p._2 ))))  filter (x=>x._2 > 0) toList

}

  val test=List(('a',2), ('b',2))

val x = test.toMap

subtract2(test, test)
Map[Char,Int]()

//(test toMap) foldLeft(Map[Char,Int]()) ((a,b)=>a)

//def sentenceAnagrams2(sentence: Sentence): List[Sentence] = {
val sentence = List("I", "Love", "You")
val s2 = List("Linux", "rulez")

val s3 = List("I", "Love", "You")
s3.permutations.toList
//Anagrams.sentenceAnagrams(s3)
//  val occurrences = Anagrams.sentenceOccurrences(sentence)
//  val combinations = Anagrams.combinations(occurrences)
//  val combinationsInDic = combinations map (a=>(a,(Anagrams.dictionaryByOccurrences get (a) getOrElse(Nil))))  filter (_._2 != Nil)
//
//  val reversedCombinations = for {
//    (k, v) <- combinationsInDic
//    w <- v
//  } yield (w->k)
//
//  def isGoodAnagram(candidate: List[(Word, Occurrences)], target:Occurrences) : Boolean = {
//    val sentence = candidate map (_._1)
//    val candidateSentenceOccurence = Anagrams.sentenceOccurrences(sentence)
//    Anagrams.subtract(target,candidateSentenceOccurence).isEmpty
//  }
//
//  val permutations = for {
//    ix<- 1 to reversedCombinations.length
//    possibleAnagram <- reversedCombinations combinations ix
//    if isGoodAnagram(possibleAnagram, occurrences)
//  }yield possibleAnagram







//}