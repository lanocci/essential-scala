sealed trait Tree {
  def sum: Int
  def double: Tree
}
final case class Node(l: Tree, r: Tree) extends Tree {
  def sum: Int = l.sum + r.sum
  def double: Tree = Node(l.double, r.double)
}
final case class Leaf(v: Int) extends Tree {
  def sum: Int = v
  def double: Tree = Leaf(v * 2)
}
