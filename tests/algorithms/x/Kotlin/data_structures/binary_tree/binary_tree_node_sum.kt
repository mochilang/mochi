data class Node(var value: Int = 0, var left: Int = 0, var right: Int = 0)
var example: MutableList<Node> = mutableListOf(Node(value = 10, left = 1, right = 2), Node(value = 5, left = 3, right = 0 - 1), Node(value = 0 - 3, left = 4, right = 5), Node(value = 12, left = 0 - 1, right = 0 - 1), Node(value = 8, left = 0 - 1, right = 0 - 1), Node(value = 0, left = 0 - 1, right = 0 - 1))
fun node_sum(tree: MutableList<Node>, index: Int): Int {
    if (index == (0 - 1)) {
        return 0
    }
    var node: Node = tree[index]!!
    return (node.value + node_sum(tree, node.left)) + node_sum(tree, node.right)
}

fun main() {
    println(node_sum(example, 0))
}
