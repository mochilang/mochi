sealed trait Tree
case object Leaf extends Tree
case class Node(left: Tree, value: Int, right: Tree) extends Tree

object tree_sum {
  def sum_tree(t: Tree): Int = t match {
    case Leaf => 0
    case Node(l,v,r) => sum_tree(l) + v + sum_tree(r)
  }
  def main(args: Array[String]): Unit = {
    val t = Node(Leaf,1,Node(Leaf,2,Leaf))
    println(sum_tree(t))
  }
}
