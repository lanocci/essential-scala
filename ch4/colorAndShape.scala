sealed trait Color {
  def r: Int
  def g: Int
  def b: Int
  val isLight: Boolean = r + g + b > 383
  val isDark: Boolean = r + g + b <= 383
}

final case object Red extends Color{
  val r = 255
  val g = 0
  val b = 0
}

final case object Yellow extends Color{
  val r = 255
  val g = 255
  val b = 0
}

final case object Pink extends Color{
  val r = 255
  val g = 192
  val b = 203
}

final case class CustomColor(r: Int, g: Int, b: Int) extends Color

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def color: Color
}

sealed trait Rectangular extends Shape {
  val sides: Int = 4
  def width: Double
  def height: Double
  val perimeter: Double = width * 2 + height * 2
  val area: Double = width * height
}

final case class Circle(r: Double, color: Color) extends Shape {
  val sides = 1
  val perimeter = r * 2 * math.Pi
  val area = r * r* math.Pi
}

final case class Rectangle(width: Double, height: Double, color: Color) extends Rectangular

final case class Square(size: Double, color: Color) extends Rectangular {
  val width, height = size
}

object Draw {
  def apply(s: Shape): String = s match {
    case Rectangle(w, h, c) => s"A ${getColor(c)} rectangle of width ${w}cm and height ${h}cm"
    case Circle(r, c) => s"A ${getColor(c)} circle of radius ${r}cm"
    case Square(s, c) => s"A ${getColor(c)}square of size ${s}cm"
  }
  def getColor(c: Color): String = c match {
    case Red => "red"
    case Yellow => "yellow"
    case Pink => "pink"
//    case CustomColor(_, _, _, _) => if(c.isLight) "light" else "dark"
    case color => if(color.isLight) "light" else "dark"
  }
}
