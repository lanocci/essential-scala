case class Cat(colour: String, food: String)

object ChipShop {
  def willServe(cat: Cat): Boolean = 
    cat match {
      case Cat(_, "chips") => true
      case Cat(_, _) => false
  }
}
