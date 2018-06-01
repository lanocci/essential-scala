def tuplized[A, B](in: (A, B)) = in._1  // _1 is a field of tuple which returns the first element of the tuple
def tuplized2[A, B](in: (A, B)) = in._2 // _2 is a field of tuple which returns the second element of the tuple
tuplized(("a", 1))
tuplized2(("a", 1))

(1, "a") match {
  case (a, b) => a + b
}
