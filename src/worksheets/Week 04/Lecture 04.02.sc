//trait Function1[A, B]{
//  def apply(x: A): B
//}
//
//(x: Int) => x * x
//
//{ class AnonFun extends Function1[Int, Int]{
//  def apply(x: Int) = x * x
//}
//  new AnonFun
//}

val f = (x: Int) => x * x
f(7)

//eta expansion



object xList {
  // List(1, 2) = List.apply(1, 2)

  def apply[T](x1: T, x2: T): lists.List[T] = new lists.Cons(x1, new lists.Cons(x2, new lists.Nil))
  def apply[T](x1: T): lists.List[T] = new lists.Cons(x1, new lists.Nil)
  def apply[T]() = new lists.Nil
}