final case class Box[A](value: A)
def generic[A, B, C](a: A, b: B, c: C): String = s"${a}, ${b}, ${c}"
