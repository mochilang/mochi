sealed interface Tree
object Leaf : Tree()
data class Node(val left: Tree, val value: Int, val right: Tree) : Tree()
fun sum_tree(t: Tree): Int {
    return when (t) {
    is Leaf -> 0
    is Node -> run {
    val left: Tree = (t as Node).left
    val value: Int = (t as Node).value
    val right: Tree = (t as Node).right
    ((sum_tree(left) as Number).toDouble() + value) + (sum_tree(right) as Number).toDouble()
}
}
}

fun main() {
    val t: Node = Node(left = Leaf, value = 1, right = Node(left = Leaf, value = 2, right = Leaf))
    println(sum_tree(t))
}
