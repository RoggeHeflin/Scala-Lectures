import scala.annotation.tailrec

def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)

def pascal(c: Int, r: Int): Int = {
  tailRecursive.factorial(c) / tailRecursive.factorial(r) / tailRecursive.factorial(c - r)
}

factorial(4)

tailRecursive.gcd(14, 21)
tailRecursive.factorial(4)
tailRecursive.factorial(-1)

pascal(4, 2)

tailRecursive.balance("(if (zero? x) max (/ 1 x))".toList)
tailRecursive.balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
tailRecursive.balance(":-)".toList)
tailRecursive.balance("())(".toList)

object tailRecursive {

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def factorial(n: Int): Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }

  def balance(chars: List[Char]): Boolean = {

    var acc: Int = 0

    @tailrec
    def loop(chars: List[Char]): Int = {

      if (chars.head.toString == "(") acc = acc + 1
      if (chars.head.toString == ")") acc = acc - 1

      if ((acc < 0) || chars.tail.isEmpty)
        acc
      else
        loop(chars.tail)
    }

    loop(chars) == 0
  }
}
