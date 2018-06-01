sealed trait Maybe[A]
final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

val perhaps1: Maybe[Int] = Empty[Int]
val perhaps2: Maybe[Int] = Full(1)
