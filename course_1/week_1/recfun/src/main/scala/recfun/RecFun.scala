package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def runningCheck(subChars: List[Char], left: Int): Boolean = {
      if (left < 0) false
      else if (subChars.isEmpty) left == 0
      else if (subChars.head == '(') runningCheck(subChars.tail, left + 1)
      else if (subChars.head == ')') runningCheck(subChars.tail, left - 1)
      else runningCheck(subChars.tail, left)
    }

    runningCheck(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty && money > 0) 0
    else if (money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
