sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(reason: String) extends Calculation
object Calculator {
  def +(calc: Calculation, y: Int) =
    calc match {
      case Success(x) => Success(x + y)
      case Failure(reason) => Failure(reason)
    }
  def -(calc: Calculation, y: Int) =
    calc match {
      case Success(x) => Success(x - y)
      case Failure(reason) => Failure(reason)
    }
  def /(calc: Calculation, y: Int) =
    calc match {
      case Success(x) =>
        y match {
          case 0 => Failure("Division by zero")
          case _ => Success(x/ y)
        }
      case Failure(reason) => Failure(reason)
    }
}
