case class Water(size: Int, source: Source, isCarbonated: Boolean)

sealed trait Source
final case object Well extends Source
final case object Tap extends Source
final case object Spring extends Source
