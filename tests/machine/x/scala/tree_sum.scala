sealed trait Tree
case object Leaf extends Tree
case class Node(left: Tree, value: Int, right: Tree) extends Tree

object tree_sum {
  def sum_tree(t: Tree): Int = t match {
    case Leaf => 0
    case Node(left, value, right) => (sum_tree(left) + (value).asInstanceOf[Int]).asInstanceOf[Int] + sum_tree(right)
  }
  
  def main(args: Array[String]): Unit = {
    val t = Node(left = Leaf, value = 1, right = Node(left = Leaf, value = 2, right = Leaf))
    println((sum_tree(t)))
  }
}
