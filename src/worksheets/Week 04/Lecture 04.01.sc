// Peano numbers

abstract class Nat{
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  override def isZero = true
  override def predecessor = throw new Error("0.predecessor")
  //override def successor = new Succ(this) refactor to base class
  override def + (that: Nat) = that
  override def - (that: Nat) = if (that.isZero) this else throw new Error("negative")
}

// n is the current number; Succ(n) represents the next number
class Succ(n: Nat) extends Nat {
  override def isZero = false

  // the predecessor of Secc(n) is just n
  override def predecessor = n

  // the successor of Secc(n) is Secc(Secc(n))... Secc(this)
  //override def successor = new Succ(this) refactor to base class

  override def + (that: Nat) = new Succ(n + that)

  override def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}

