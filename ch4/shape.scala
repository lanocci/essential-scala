trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

case class Circle(r: Double) extends Shape{
  val sides = 1
  val perimeter = r * 2 * math.Pi
  val area = r * r * math.Pi
}

case class Rectangle(height: Double, width: Double) extends Shape {
  val sides = 4
  val perimeter = height * 2 + width * 2
  val area = height * width
}

case class Square(sideLength: Double) extends Shape {
  val sides = 4
  val perimeter = sideLength * 4
  val area = sideLength * sideLength
}
