trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

trait Rectangular extends Shape {
  def width: Double
  def height: Double
  val sides: Int = 4
  val perimeter: Double = width * 2 + height * 2
  val area: Double = width * height
}

case class Circle(r: Double) extends Shape{
  val sides = 1
  val perimeter = r * 2 * math.Pi
  val area = r * r * math.Pi
}

case class Rectangle(height: Double, width: Double) extends Rectangular

case class Square(sideLength: Double) extends Rectangular {
  val width = sideLength
  val height = sideLength
}
