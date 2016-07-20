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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], depth: Int): Boolean = chars match {
      case Nil => depth == 0
      case '(' :: t => loop(t, depth + 1)
      case ')' :: t => if (depth > 0) loop(t, depth - 1) else false
      case _ :: t => loop(t, depth)
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0 else coins match {
        case Nil => if (money == 0) 1 else 0
        case h :: t => loop(money - h, coins) + loop(money, t)
      }
    }
    loop(money, coins.sorted)
  }
}
