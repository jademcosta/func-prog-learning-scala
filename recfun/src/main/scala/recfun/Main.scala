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
    def isOnTheEdgeColumn() = {
      c == 0 || (c == r)
    }
    
    def isFirstRow() = {
      r == 0
    }
    
    if(isOnTheEdgeColumn() || isFirstRow()) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
//    var count = 0
    
    def alterCountIfIsParenthesis(aChar: Char, count: Int): Int = {
      if(aChar == '(') {
        count + 1
      } else if(aChar == ')') {
        count - 1
      } else {
        count
      }
    }
    
    def isBalanced(charsSliced: List[Char], count: Int): Boolean = {
      
      if(count < 0) {
        return false
      }
      
      if(charsSliced.isEmpty) {
        if(count == 0) return true else return false
      }
      
      var c = alterCountIfIsParenthesis(charsSliced.head, count)
       
      isBalanced(charsSliced.tail, c)
    }
    
    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    var sortedCoins = coins.sorted
    
    if(sortedCoins.isEmpty || (money - sortedCoins.head) < 0) {
      return 0
    }  else if((money - sortedCoins.head) == 0) {
      return 1
    } else {
      countChange(money - sortedCoins.head, sortedCoins) + 
      countChange(money, sortedCoins.tail)
    }
  }
}
