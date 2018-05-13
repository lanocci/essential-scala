case class Counter(count: Int = 0) {
  def inc = Counter(count + 1)
  def dec = Counter(count - 1)
  def adjust(a: Adder) = copy(count = a(count))
}

class Adder(amount: Int) {
  def apply(in: Int) = in + amount
}
