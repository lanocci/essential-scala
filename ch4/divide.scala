sealed trait DivisionResult

final case class Finite(result: Int) extends DivisionResult
final case object Infinite extends DivisionResult

object divide {
  def apply(x: Int, y: Int): DivisionResult = {
    if(y == 0) Infinite else Finite(x / y)
  }
}

divide(1, 0) match {
  case Finite(value) => s"It's Finite: ${value}"
  case Infinite => s"It's Infinite"
}
