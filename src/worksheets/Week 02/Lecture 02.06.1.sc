val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x + y
x < y


class Rational(n: Int, d: Int) {
  require(d != 0, "denominator must be non-zero") // pre-condition on the caller of the function
  require(d > 0, "denominator must be positive")
  assert(d >= 0, "denominator must be positive") // check code of the function itself

  val g: Int = gcd(n, d)

  def gcd(n: Int, d: Int): Int = if (d == 0) n else gcd(d, n % d)

  def -(that: Rational): Rational = this + -that

  def +(that: Rational): Rational =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def unary_- : Rational = new Rational(-this.numer, this.denom)

  def this(x: Int) = this(x, 1)

  override def toString: String = this.numer + "/" + this.denom

  def numer: Int = n / g

  def denom: Int = d / g

  def max(that: Rational): Rational = if (this < that) that else this

  def <(that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom

}