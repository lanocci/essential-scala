sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

sealed trait Rectangular extends Shape {
  val sides: Int = 4
  def width: Double
  def height: Double
  val perimeter: Double = width * 2 + height * 2
  val area: Double = width * height
}

final case class Circle(r: Double) extends Shape {
  val sides = 1
  val perimeter = r * 2 * math.Pi
  val area = r * r* math.Pi
}

final case class Rectangle(width: Double, height: Double) extends Rectangular

final case class Square(size: Double) extends Rectangular {
  val width, height = size
}

object Draw {
  def apply(s: Shape): String = s match {
    case Rectangle(w, h) => s"A rectangle of width ${w}cm and height ${h}cm"
    case Circle(r) => s"A circle of radius ${r}cm"
    case Square(s) => s"A square of size ${s}cm"
  }
}
