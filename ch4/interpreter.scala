sealed trait Calculation
final case class Success(result: Double) extends Calculation
final case class Failure(reason: String) extends Calculation

sealed trait Expression {
  def eval(): Calculation =
    this match {
      case Addition(l, r) => {
        l.eval match {
          case Failure(m) => Failure(m)
          case Success(r1) => {
            r.eval match {
              case Success(r2) => Success(r1 + r2)
              case Failure(m) => Failure(m)
            }
          }
        }
      }
      case Subtraction(l, r) => {
        l.eval match {
          case Failure(m) => Failure(m)
          case Success(r1) => {
            r.eval match {
              case Success(r2) => Success(r1  - r2)
              case Failure(m) => Failure(m)
            }
          }
        }
      }
      case Division(l, r) => {
        l.eval match {
          case Failure(m) => Failure(m)
          case Success(r1) => {
            r.eval match {
              case Success(r2) => if(r2 == 0) Failure("Division by zero") else Success(r1 / r2)
              case Failure(m) => Failure(m)
            }
          }
        }
      }
      case SquareRoot(v) => {
        v.eval match {
          case Success(v) => if(v < 0) Failure("Square root of negative number") else Success(Math.sqrt(v))
          case Failure(m) => Failure(m)
        }
      }
      case Number(v) => Success(v)
    }
}
final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Number(value: Double) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
