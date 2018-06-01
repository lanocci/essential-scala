sealed trait Maybe[A] {
  def fold[B](empty: B, f: (a: A) => B) =
    this match {
      case Full(v) => f(v)
      case Empty => empty
    }
}

final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

val perhaps1: Maybe[Int] = Empty[Int]
val perhaps2: Maybe[Int] = Full(1)
