def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

// use a function to transform (Lecture 2.1)
def sum_A(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum_A(f, a + 1, b)

// function must be submitted as an argument
def sumInts_A(a: Int, b: Int): Int = sum_A(x => x, a, b)
def sumCubes_A(a: Int, b: Int): Int = sum_A(x => x * x * x, a, b)
def sumFacts_A(a: Int, b: Int): Int = sum_A(x => fact(x), a, b)

sumCubes_A(1, 10)
sumFacts_A(1, 10)

// re-write and return a function:
def sumInts = sum_B(x => x)
def sumCubes = sum_B(x => x * x * x)
def sumFacts = sum_B(x => fact(x))

def sum_B(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)

  sumF
}

sumCubes(1, 10)
sumFacts(1, 10)

sum_B(cube)(1, 10)
sum_B(fact)(1, 10)

// refactor sum_A function
def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)

// I only see a syntax difference, where/what/why is the benefit of this syntax?


sum(cube)(1, 10)
sum(fact)(1, 10)

// write a product function (Like sum)
def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

product(x => x * x)(3, 4)

def factorial(n: Int) = product(x => x)(1, n)

factorial(5)
fact(5)

// write generic function for sum and product
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def mrSum(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x + y, 0)(a, b)

def mrProduct(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

mrSum(x => x)(3, 4)
mrSum(id)(3, 4)
mrProduct(x => x * x)(3, 4)

