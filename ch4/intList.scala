sealed trait IntList
final case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList
def sum(list: IntList): Int = 
  list match {
    case Pair(h, t) => h + sum(t)
    case End => 0
  }
val example = Pair(1, Pair(2, Pair(3, End)))
assert(sum(example) == 6)
assert(sum(example.tail) == 5)
assert(sum(End) == 0)
