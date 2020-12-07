package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
      print( countChange(6, List(25,10,5,2,1)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if( c == 0 || r == c)  1 else pascal( c-1, r-1) + pascal( c, r-1)

  def balance_count(chars: List[Char], count: Int): Boolean =
  {
    if(chars.isEmpty || count < 0) return (count == 0)
    else if(chars.head == '(') balance_count(chars.tail,count + 1)
    else if (chars.head == ')') balance_count(chars.tail, count -1)
    else balance_count(chars.tail,count )
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = balance_count(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    {
      //println("money " + money + " coins " + coins)
      if(money == 0)
        1
      else if(money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else
        0
    }
}
