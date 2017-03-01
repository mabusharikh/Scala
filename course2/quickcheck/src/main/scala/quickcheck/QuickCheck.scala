package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }


  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)
    findMin(h2) == List(a, b).min
  }

  property("delMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == Nil
  }

  property("minOfMeld") = forAll { (a: H, b: H) =>
    val h = meld(a, b)
    findMin(h) == List(findMin(a), findMin(b)).min
  }

  property("trueMin") = forAll(listOf(arbitrary[Int])) { a =>
    if (a.isEmpty)
      true
    else {
      val h = (a.foldLeft(empty)((x: H, y: Int) => insert(y, x)))
      findMin(h) == (a min)
    }
  }

  property("contDeleteInOrder") = forAll { a: H =>
    def verify(curMin: Int, h: H): Boolean = {
      if (h == Nil)
        true
      else {
        val m = findMin(h)
        if (m > curMin)
          false
        verify(m, deleteMin(h))
      }
    }

    verify(findMin(a), a)
  }


  property("meldInOrder") = forAll { (a: H, b: H) =>
    val m1 = meld(a, b)
    val m2 = meld(insert(findMin(a), b), deleteMin(a))

    def getSeq(x: H): List[Int] =
      if (isEmpty(x)) Nil
      else findMin(x) :: getSeq(deleteMin(x))

    getSeq(m1) == getSeq(m2)
  }



}