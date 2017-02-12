val x = 0

val result = {
  val x = f(3)
  x * x
} + x

def f(y: Int) = y + 1
