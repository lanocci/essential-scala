trait Feline {
  def colour: String
  def sound: String
}

case class Cat(colour: String, food: String) extends Feline {
  val sound = "meow"
}

case class Tiger(colour: String) extends Feline {
  val sound = "roar"
}

case class Lion(colour: String, maneSize: Int) extends Feline {
  val sound = "roar"
}

case class Panther(colour: String) extends Feline {
  val sound = "roar"
}
