sealed trait Food
final case object Antelope extends Food
final case object TigerFood extends Food
final case object Licorice extends Food
final case class CatFood(food: String) extends Food

sealed trait Feline {
  def dinner: Food
}

final case class Lion() extends Feline {
  val dinner = Antelope
}

final case class Tiger() extends Feline {
  val dinner = TigerFood
}

final case class Panther() extends Feline {
  val dinner = Licorice
}

final case class Cat(favouriteFood: String) extends Feline {
  val dinner = CatFood(favouriteFood)
}
