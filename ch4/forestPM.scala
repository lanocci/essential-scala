sealed trait Tree {
  def sum: Int =
    this match {
      case Node(l, r) => l.sum + r.sum
      case Leaf(v) => v
    }
  def double: Tree =
    this match {
      case Node(l, r) => Node(l.double, r.double)
      case Leaf(v) => Leaf(v * 2)
    }
}

final case class Node(l: Tree, r: Tree) extends Tree
final case class Leaf(v: Int) extends Tree
