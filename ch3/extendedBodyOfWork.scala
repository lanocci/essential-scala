class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name(): String = s"$firstName $lastName"
}

object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int): Director =
    new Director(firstName, lastName, yearOfBirth)

  def older(da: Director, db: Director): Director =
    if(da.yearOfBirth <= db.yearOfBirth) return da else return db
}

class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
  def directorsAge = yearOfRelease - director.yearOfBirth
  def isDirectedBy(d: Director) = d.name == director.name
  def copy(name: String = this.name, yearOfRelease: Int = this.yearOfRelease, imdbRating: Double = this.imdbRating, director: Director = this.director): Film =
    new Film(name, yearOfRelease, imdbRating, director)
}

object Film {
  def apply(name: String, yearOfRelease: Int, imdbRating: Double, director: Director): Film =
    new Film(name, yearOfRelease, imdbRating, director)

  def highestRating(fa: Film, fb: Film): Double =
    if(fa.imdbRating > fb.imdbRating) return fa.imdbRating else return fb.imdbRating

  def oldestDirectorAtTheTime(fa: Film, fb: Film): Director =
    if(fa.directorsAge > fb.directorsAge) fa.director else fb.director
}

