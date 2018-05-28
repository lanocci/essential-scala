sealed trait IntList {
  def fold[A](end: A, f: (Int, A) => A): A =
    this match {
      case End() => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }
  def length: Int = fold[Int](0, (_, t) => 1 + t)
  def sum: Int = fold[Int](0, (h, t) => h + t)
  def product: Int = fold[Int](1, (h, t) => h * t)
  def double: IntList = fold[IntList](End(), (h, t) => Pair(h * 2, t))
}
final case class End() extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList

val example = Pair(1, Pair(2, Pair(3, End())))
assert(example.length == 3)
assert(example.sum == 6)
assert(example.product == 6)
