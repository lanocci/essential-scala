import scala.annotation.tailrec

sealed trait IntList
final case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList
@tailrec
def sum(list: IntList, total: Int = 0): Int =
  list match {
    case End => total
    case Pair(hd, tl) => sum(tl, total + hd)
  }
