case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
  def name() = s"$firstName $lastName"
}

object Director {
  def older(da: Director, db: Director) = if (da.yearOfBirth <= db.yearOfBirth) da else db
}

case class Film(name: String, yearOfRelease: Int, imdbRating: Double, director: Director) {
  def directorsAge = yearOfRelease - director.yearOfBirth
  def isDirectedBy(d: Director) = d.name == director.name
}

object Film  {
  def newer(fa: Film, fb: Film) = if (fa.yearOfRelease > fb.yearOfRelease) fa else fb
  def highestRating(fa: Film, fb: Film): Double = {
    val rateA = fa.imdbRating
    val rateB = fb.imdbRating
    if (rateA > rateB) rateA else rateB
  }
  def oldestDirectorAtTheTime(fa: Film, fb: Film): Director = {
    if (fa.directorsAge > fb.directorsAge) fa.director else fb.director
  }
}
