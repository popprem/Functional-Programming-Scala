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
        if (c == 0 || c == r) {
          1
        } else {
          pascal(c - 1, r - 1) + pascal(c, r - 1)
        }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def check(unbalanced:Int, chars: List[Char]): Boolean = {

        if (unbalanced < 0) {
          return false;
        }

        if (chars.isEmpty) {
          return  if (unbalanced == 0)return true else false
        }

        val c = chars.head
        val newUnbalanced = {
          if (c == '(') unbalanced + 1
          else if (c == ')') unbalanced - 1
          else unbalanced
        }
        check(newUnbalanced, chars.tail)
      }

      check(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def count(m: Int, c: List[Int]) : Int = {
        if (coins.isEmpty) if (money == 0) 1 else 0
        else if (m - c.head == 0) 1
        else if (m - c.head < 0) 0
        else countChange(m - c.head, c) + countChange(m, c.tail)
      }
      count(money, coins.sorted)
    }
  }
