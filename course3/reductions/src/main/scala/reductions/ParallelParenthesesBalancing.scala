package reductions

import java.util.function.IntUnaryOperator

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 1000000
    val chars = new Array[Char](length)
    val threshold = 1000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  def classifyChar(ch: Char): Int = ch match{
    case '('=>1
    case ')'=>(-1)
    case _=>0
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    def check(chx: List[Char], acc:Int) :Boolean={
      chx match {
        case Nil=>acc==0
        case x::xs=>acc + classifyChar(x) match {
          case x if x < 0 => false
          case x=> check(xs, x)
        }
      }
    }

    check(chars.toList, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, posCount: Int, negCount: Int) : (Int, Int) = {
      var p = posCount
      var n = negCount

      var i= idx
      while (i < until) {
        chars(i) match{
          case '(' =>p+=1
          case ')'=> if (p > 0) p-=1 else n+=1
          case _=>
        }

        i= i+1
      }
      (p,n)

    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from < threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from)/2
        val (l,r) = parallel(reduce(from, mid), reduce(mid, until))
        val i = math.min(l._1, r._2)
        (l._1 + r._1 - i, l._2 + r._2 - i)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
