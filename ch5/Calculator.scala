// Excercise 5.6.6.2

sealed trait Sum[+A, +B] {
  def fold[C](error: A => C, success: B => C): C =
    this match {
      case Failure(v) => error(v)
      case Success(v) => success(v)
    }
  def map[C](f: B => C): Sum[A, C] =
    this match {
      case Failure(v) => Failure(v)
      case Success(v) => Success(f(v))
    }
  def flatmap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
    this match {
      case Failure(v) => Failure(v)
      case Success(v) => f(v)
    }
}

final case class Failure[A](value: A) extends Sum[A, Nothing]
final case class Success[B](value: B) extends Sum[Nothing, B]

sealed trait Expression {
  def eval: Sum[String, Double] =
    this match {
      case Number(v) => Success(v)
      case Addition(l, r) => lift(l, r, (left, right) => Success(left + right))
      case Subtraction(l, r) => lift(l, r, (left, right) => Success(left - right))
      case Division(l, r) => {
        lift(l, r, (left, right) => {
          if(right == 0) Failure("division by zero")
          else Success(left / right)
        })
      }
      case SquareRoot(v) => {
        v.eval.flatmap { value =>
          if (value < 0) Failure("Square root of negative number")
          else Success(Math.sqrt(value))
        }
      }
    }
  def lift(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]): Sum[String, Double] =
    l.eval.flatmap{ left =>
      r.eval.flatmap { right =>
        f(left, right)
      }
    }
}
final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
final case class Number(value: Double) extends Expression
