val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

class Rational(n: Int, d: Int) {

  def sub(that: Rational): Rational = add(that.neg)

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)

  override def toString: String = numer + "/" + denom

  def numer: Int = n

  def denom: Int = d
}

x.numer
x.denom

x.add(y)
x.add(y).toString()

x.sub(y).sub(z)

