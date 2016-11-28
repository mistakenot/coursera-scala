package recfun

object Main {
  def main(args: Array[String]) {
    val ans = countChange(4, List(1, 2))
    println(ans)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == r || c == 0) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def inner (depth: Int, c: List[Char]): Boolean =
        if (c.isEmpty) depth == 0
        else if (c.head == '(') inner(depth + 1, c.tail)
        else if (c.head == ')')
          if (depth == 0) false else inner(depth - 1, c.tail)
        else inner(depth, c.tail)

      inner(0, chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
