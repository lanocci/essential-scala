sealed trait TrafficLight {
  def next: TrafficLight =
    this match {
      case Red => Green
      case Yellow => Red
      case Green => Yellow
    }
}
final case object Red extends TrafficLight
final case object Yellow extends TrafficLight
final case object Green extends TrafficLight
