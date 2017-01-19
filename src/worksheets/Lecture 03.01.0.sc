// superclass
abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

// subclass
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((this.left union this.right) union other) incl this.elem

  override def toString: String = "{" + left + elem + right + "}"
}

// subclass; can change to a singleton object
//class Empty extends IntSet {
//  def contains(x: Int): Boolean = false
//
//  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//
//  override def toString: String = "."
//}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString: String = "."

  def union(other: IntSet): IntSet = other
}

val e0 = Empty
val e1 = e0 incl 7
val e2 = e1 incl 5
val e3 = e1 incl 12

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
