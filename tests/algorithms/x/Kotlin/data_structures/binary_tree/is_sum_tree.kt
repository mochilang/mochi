fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

data class Node(var value: Int = 0, var left: Int = 0, var right: Int = 0)
fun tree_sum(nodes: MutableList<Node>, idx: Int): Int {
    if (idx == (0 - 1)) {
        return 0
    }
    var node: Node = nodes[idx]!!
    return (node.value + tree_sum(nodes, node.left)) + tree_sum(nodes, node.right)
}

fun is_sum_node(nodes: MutableList<Node>, idx: Int): Boolean {
    var node: Node = nodes[idx]!!
    if ((node.left == (0 - 1)) && (node.right == (0 - 1))) {
        return true
    }
    var left_sum: Int = (tree_sum(nodes, node.left)).toInt()
    var right_sum: Int = (tree_sum(nodes, node.right)).toInt()
    if (node.value != (left_sum + right_sum)) {
        return false
    }
    var left_ok: Boolean = true
    if (node.left != (0 - 1)) {
        left_ok = ((is_sum_node(nodes, node.left)) as Boolean)
    }
    var right_ok: Boolean = true
    if (node.right != (0 - 1)) {
        right_ok = ((is_sum_node(nodes, node.right)) as Boolean)
    }
    return ((left_ok && right_ok) as Boolean)
}

fun build_a_tree(): MutableList<Node> {
    return mutableListOf(Node(value = 11, left = 1, right = 2), Node(value = 2, left = 3, right = 4), Node(value = 29, left = 5, right = 6), Node(value = 1, left = 0 - 1, right = 0 - 1), Node(value = 7, left = 0 - 1, right = 0 - 1), Node(value = 15, left = 0 - 1, right = 0 - 1), Node(value = 40, left = 7, right = 0 - 1), Node(value = 35, left = 0 - 1, right = 0 - 1))
}

fun build_a_sum_tree(): MutableList<Node> {
    return mutableListOf(Node(value = 26, left = 1, right = 2), Node(value = 10, left = 3, right = 4), Node(value = 3, left = 0 - 1, right = 5), Node(value = 4, left = 0 - 1, right = 0 - 1), Node(value = 6, left = 0 - 1, right = 0 - 1), Node(value = 3, left = 0 - 1, right = 0 - 1))
}

fun test_non_sum_tree(): Unit {
    var tree: MutableList<Node> = build_a_tree()
    expect(is_sum_node(tree, 0) == false)
}

fun test_sum_tree(): Unit {
    var tree: MutableList<Node> = build_a_sum_tree()
    expect(is_sum_node(tree, 0) == true)
}

fun main() {
    test_non_sum_tree()
    test_sum_tree()
}
