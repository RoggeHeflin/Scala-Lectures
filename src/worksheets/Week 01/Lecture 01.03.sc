def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (!(money > 0 && coins.nonEmpty))
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
}

countChange(40, List(20, 10, 5)) //== 9
countChange(4, List(1, 2)) //== 3
countChange(300, List(5, 10, 20, 50, 100, 200, 500)) //== 1022
countChange(301, List(5, 10, 20, 50, 100, 200, 500)) //== 0
countChange(300, List(500, 5, 50, 100, 20, 200, 10)) //== 1022