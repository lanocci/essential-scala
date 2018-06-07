sealed trait Result[A]
final case class Success[A](result: A) extends Result[A]
final case class Failure[A](reason: String) extends Result[A]

sealed trait LinkedList[A] {
  def length: Int =
    this match {
      case End() => 0
      case Pair(h, t) => 1 + t.length
    }
  def contains[B](comparison: B): Boolean =
    this match {
      case Pair(h, t) => if(h == comparison) true else t.contains(comparison)
      case End() => false
    }
//  def apply(index: Int): A =
//    this match {
//      case Pair(h, t) => if(index == 0) h else t(index - 1)
//      case End() => throw new Exception("index out of bounds exception")
//    }
  def apply(index: Int): Result[A] =
    this match {
      case Pair(h, t) => if(index == 0) Success(h) else t(index -1)
      case End() => Failure("Index out of bounds")
    }
  def map[B](fn: A => B): LinkedList[B] =
    this match {
      case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
      case End() => End[B]()
    }
}
final case class End[A]() extends LinkedList[A]
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

val example = Pair(1, Pair(2, Pair(3, End())))
assert(example.length == 3)
assert(example.tail.length == 2)
assert(End().length == 0)
assert(example.contains(3) == true)
assert(example.contains(4) == false)
assert(End().contains(0) == false)
// assert(example(0) == 1)
// assert(example(1) == 2)
// assert(example(2) == 3)
// assert(try {
//   example(3)
//   false
// } catch {
//   case e: Exception => true
// })
assert(example(0) == Success(1))
assert(example(1) == Success(2))
assert(example(2) == Success(3))
assert(example(3) == Failure("Index out of bounds"))

//def double = (x: Int) => x * 2
//def add1 = (x: Int) => x + 1
//assert(example.map(double) == Pair(2, Pair(4, Pair(6, End()))))
//assert(example.map(add1) == Pair(2, Pair(3, Pair(4, End()))))
assert(example.map(_ * 2) == Pair(2, Pair(4, Pair(6, End()))))
assert(example.map(_ + 1) == Pair(2, Pair(3, Pair(4, End()))))

val exampleDouble = Pair(3.0, Pair(6.0, Pair(9.0, End())))
// def divideByThree = (x: Double) => x / 3
// assert(exampleDouble.map(divideByThree) == Pair(1.0, Pair(2.0, Pair(3.0, End()))))
assert(exampleDouble.map(_ / 3) == Pair(1.0, Pair(2.0, Pair(3.0, End()))))
