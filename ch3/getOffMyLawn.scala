case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
  def name = s"$firstName $lastName"
}

case class Film(name: String, yearOfRelease: Int, imdbRating: Double, director: Director)

object Dad {
  def rate(film: Film): Double = {
    film match {
      case Film(_, _, _, Director("Clint", "Eastwood", _)) => 10.0
      case Film(_, _, _, Director("John", "McTiernan", _)) => 7.0
      case Film(_, _, _, _) => 3.0
    }
  }
}
