// これだとAny型になってしまってよくない
// def intOrString(input: Boolean) = 
//   if(input == true) 123 else "abc"

def intOrString(input: Boolean): Sum[Int, String] =
  if(input == true) {
    Left[Int, String](123)
  } else {
    Right[Int, String]("abc")
  }

sealed trait Sum[A, B] {
  def fold[C](left: A => C, right: B => C) =
    this match {
      case Left(a) => left(a)
      case Right(b) => right(b)
    }
}
final case class Left[A, B](value: A) extends Sum[A, B]
final case class Right[A, B](value: B) extends Sum[A, B]

Left[Int, String](1).value
Right[Int, String]("foo").value
val sum: Sum[Int, String] = Right("foo")
sum match {
  case Left(x) => x.toString
  case Right(x) => x
}
