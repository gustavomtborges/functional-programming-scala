package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(col: Int, row: Int): Int = {
    if (col == 0 || col == row) 1
    else {
      pascal(col - 1, row - 1) + pascal(col, row - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def findClose(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) count == 0
      else if (chars.head == '(')
        findClose(chars.tail, count + 1)
      else if (chars.head == ')')
        count > 0 && findClose(chars.tail, count - 1)
      else
        findClose(chars.tail, count)
    }

    findClose(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
