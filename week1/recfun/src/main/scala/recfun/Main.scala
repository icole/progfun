package recfun

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
      def createrow(prevrow: List[Int], col: Int = 1, newrow: List[Int] = List(1)): List[Int] =
        if (col == prevrow.length) 1 :: newrow
        else createrow(prevrow, col+1, (prevrow(col-1) + prevrow(col)) :: newrow)

      def createrows(rows: Int, prevrow: List[Int] = List(1)) : List[Int] =
        if (rows == 0) prevrow else createrows(rows-1, createrow(prevrow))

      createrows(r)(c)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countbalance(c: Int, chars: List[Char]): Int = {
        if (c < 0 || chars.isEmpty)
          c
        else if (chars.head == '(')
          countbalance(c+1, chars.tail)
        else if (chars.head == ')')
          countbalance(c-1, chars.tail)
        else
          countbalance(c, chars.tail)
      }
      countbalance(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
