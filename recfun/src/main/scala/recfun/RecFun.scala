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
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def balanceLoop(currentBalance: Int, chars: List[Char]): Boolean = {
      if (currentBalance < 0) false // fail early if out of order
      else if (chars.isEmpty) currentBalance == 0 // whole List processed
      else {
        val currentChar = chars.head
        if (currentChar == '(') balanceLoop(currentBalance + 1, chars.tail)
        else if (currentChar == ')') balanceLoop(currentBalance - 1, chars.tail)
        else balanceLoop(currentBalance, chars.tail)
      }

    }
    balanceLoop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def changeLoop(currentCount: Int, money: Int, coins: List[Int]): Int = {
//      println(s"$currentCount -> $money -> $coins")
      if (money == 0) currentCount + 1
      else if (money < 0 || coins.isEmpty) currentCount
      else changeLoop(currentCount, money - coins.head, coins) + changeLoop(currentCount, money, coins.tail)
    }

    if(money <= 0 || coins.isEmpty) 0 else
    changeLoop(0, money, coins)
  }
}
