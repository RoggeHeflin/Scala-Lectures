import scala.annotation.tailrec

def sumInts_X(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a + 1, b)

def sumCubes_X(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

// Generic

def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def factorial(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(acc * n, n - 1)

  loop(1, n)
}
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

def sumInts(a: Int, b: Int): Int = sum(id, a, b)
def sumCubes(a: Int, b: Int): Int = sum(cube, a, b)
def sumFactorials(a: Int, b: Int): Int = sum(factorial, a, b)

sumInts(2, 5)
sumCubes(2, 5)
sumFactorials(2, 5)

def sumInts2(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes2(a: Int, b: Int) = sum(x => x * x * x, a, b)

def sumLarge(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }

  loop(a, 0)
}

sumLarge(x => x * x, 3, 5)
