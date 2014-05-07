package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == r) 1
    else if (c == 0) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def process(chars: List[Char], openCount: Integer):Boolean ={
      if (chars.isEmpty && openCount == 0)
        true
      else if (chars.isEmpty && openCount != 0)
        false
      else if (chars.head == '(')
        process(chars.tail, openCount+1)
      else if (chars.head == ')' && openCount-1 >= 0)
        process(chars.tail, openCount-1)
      else if (chars.head ==')' && openCount-1 < 0)
        false
      else
        process(chars.tail, openCount)

    }

    process(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted

    def process(myCoins: List[Int], mySum: Int): Int = {
      if (myCoins.isEmpty)
        0
      else if (mySum + myCoins.head < money)
        process(myCoins, mySum+myCoins.head)  + process(myCoins.tail, mySum)
      else if (mySum + myCoins.head == money)
        1
      else
        0
    }

    process(sortedCoins, 0)
  }
}
