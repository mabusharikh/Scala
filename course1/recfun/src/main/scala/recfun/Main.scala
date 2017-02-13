package recfun

import com.sun.xml.internal.messaging.saaj.packaging.mime.util.QEncoderStream

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
    //    print(balance("(frikking()".toList))
    println(countChange(4, List(1, 2, 3)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def classifyChar(ch: Char): Int =
      if (ch == '(') 1
      else if (ch == ')') -1
      else 0

    def check(ch: Char, chars: List[Char], good: Int): Boolean = {
      val newGood = good + classifyChar(ch)
      if (newGood < 0) false
      else if (chars.isEmpty) newGood == 0
      else check(chars.head, chars.tail, newGood)
    }

    if (chars.isEmpty) true
    else check(chars.head, chars.tail, 0)
  }

  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    def evaluateChanges(moneySoFar:Int, currentCoin: Int, possibleChange: List[Int], changesSoFar: Int): Int ={
      if (moneySoFar + currentCoin == money) {
        changesSoFar + 1
      }
      else if (moneySoFar + currentCoin > money && !possibleChange.isEmpty) {
        evaluateChanges(moneySoFar, possibleChange.head, possibleChange.tail, changesSoFar)
      }
      else if (moneySoFar + currentCoin < money) {
        if(possibleChange.length <= 1) {
          evaluateChanges(moneySoFar + currentCoin, currentCoin, possibleChange, changesSoFar)
        }
        else{
          evaluateChanges(moneySoFar + currentCoin, currentCoin, possibleChange, changesSoFar)  + evaluateChanges(moneySoFar, possibleChange.tail.head, possibleChange.tail, changesSoFar)
        }
      }
      else {
        changesSoFar
      }

    }

    if(coins.isEmpty) return 0

    val sortedCoils = coins.sortBy(x=>x)
    evaluateChanges(0, sortedCoils.head, sortedCoils, 0)
  }

}


