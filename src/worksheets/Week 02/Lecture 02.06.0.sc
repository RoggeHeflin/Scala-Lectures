val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
//val strange = new Rational(1, 0)

x.numer
x.denom

x.add(y)
x.less(y)
x.max(y)

//x.sub(y).sub(z)

x add y
x less y
x max y

new Rational(2)

new Rational(1, 2).numer
new Rational(1, 2).less(new Rational(2, 3))

class Rational(n: Int, d: Int) {
  require(d != 0, "denominator must be non-zero") // pre-condition on the caller of the function
  require(d > 0, "denominator must be positive")
  assert(d >= 0, "denominator must be positive") // check code of the function itself

  val g: Int = gcd(n, d)

  def gcd(n: Int, d: Int): Int = if (d == 0) n else gcd(d, n % d)

  def this(x: Int) = this(x, 1)

  def sub(that: Rational): Rational = add(that.neg)

  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def neg: Rational = new Rational(-this.numer, this.denom)

  override def toString: String = this.numer + "/" + this.denom

  def numer: Int = n / g

  def denom: Int = d / g

  def max(that: Rational): Rational = if (this.less(that)) that else this

  def less(that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom

}