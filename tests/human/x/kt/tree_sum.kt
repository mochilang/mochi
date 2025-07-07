sealed class Tree
object Leaf : Tree()
data class Node(val left: Tree, val value: Int, val right: Tree) : Tree()

fun sumTree(t: Tree): Int = when (t) {
    is Leaf -> 0
    is Node -> sumTree(t.left) + t.value + sumTree(t.right)
}

fun main() {
    val t = Node(Leaf, 1, Node(Leaf, 2, Leaf))
    println(sumTree(t))
}
