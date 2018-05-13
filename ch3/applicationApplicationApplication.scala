case class Person(firstName: String, lastName: String) {
  def name = s"$firstName $lastName"
}

object Person {
  def apply(name: String) = {
    val parts = name.split(" ")
    apply(parts(0), parts(1))
  }
}
