sealed trait Tree[A] {
  def fold[B](node: (B, B) => B)(leaf: A => B): B
//  def toText: String = this.fold[String]((x: String, y: String) => x + y, (z: String) => z)
}
final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def fold[B](node: (B, B) => B)(leaf: A => B): B =
    node(left.fold(node)(leaf), right.fold(node)(leaf))
}
final case class Leaf[A](value: A) extends Tree[A] {
  def fold[B](node: (B, B) => B)(leaf: A => B): B =
    leaf(value)
}

val tree: Tree[String] =
  Node(Node(Leaf("To"), Leaf("iterate")),
       Node(Node(Leaf("is"), Leaf("human,")),
            Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

tree.fold[String]((a, b) => a + " " + b)(str => str)
