import java.math.BigInteger

data class Node(var _val: Int = 0, var left: Int = 0, var right: Int = 0)
data class Info(var is_bst: Boolean = false, var min_val: Int = 0, var max_val: Int = 0, var total: Int = 0, var best: Int = 0)
fun min_int(a: Int, b: Int): Int {
    if (a < b) {
        return a
    }
    return b
}

fun max_int(a: Int, b: Int): Int {
    if (a > b) {
        return a
    }
    return b
}

fun solver(nodes: MutableList<Node>, idx: Int): Info {
    if (idx == (0 - 1)) {
        return Info(is_bst = true, min_val = 2147483647, max_val = ((0).toLong() - 2147483648L).toInt(), total = 0, best = 0)
    }
    var node: Node = nodes[idx]!!
    var left_info: Info = solver(nodes, node.left)
    var right_info: Info = solver(nodes, node.right)
    var current_best: Int = (max_int(left_info.best, right_info.best)).toInt()
    if (((((left_info.is_bst && right_info.is_bst as Boolean)) && (left_info.max_val < node._val) as Boolean)) && (node._val < right_info.min_val)) {
        var sum_val: Int = ((left_info.total + right_info.total) + node._val).toInt()
        current_best = max_int(current_best, sum_val)
        return Info(is_bst = true, min_val = min_int(left_info.min_val, node._val), max_val = max_int(right_info.max_val, node._val), total = sum_val, best = current_best)
    }
    return Info(is_bst = false, min_val = 0, max_val = 0, total = 0, best = current_best)
}

fun max_sum_bst(nodes: MutableList<Node>, root: Int): Int {
    var info: Info = solver(nodes, root)
    return info.best
}

fun user_main(): Unit {
    var t1_nodes: MutableList<Node> = mutableListOf(Node(_val = 4, left = 1, right = 0 - 1), Node(_val = 3, left = 2, right = 3), Node(_val = 1, left = 0 - 1, right = 0 - 1), Node(_val = 2, left = 0 - 1, right = 0 - 1))
    println(max_sum_bst(t1_nodes, 0))
    var t2_nodes: MutableList<Node> = mutableListOf(Node(_val = 0 - 4, left = 1, right = 2), Node(_val = 0 - 2, left = 0 - 1, right = 0 - 1), Node(_val = 0 - 5, left = 0 - 1, right = 0 - 1))
    println(max_sum_bst(t2_nodes, 0))
    var t3_nodes: MutableList<Node> = mutableListOf(Node(_val = 1, left = 1, right = 2), Node(_val = 4, left = 3, right = 4), Node(_val = 3, left = 5, right = 6), Node(_val = 2, left = 0 - 1, right = 0 - 1), Node(_val = 4, left = 0 - 1, right = 0 - 1), Node(_val = 2, left = 0 - 1, right = 0 - 1), Node(_val = 5, left = 7, right = 8), Node(_val = 4, left = 0 - 1, right = 0 - 1), Node(_val = 6, left = 0 - 1, right = 0 - 1))
    println(max_sum_bst(t3_nodes, 0))
}

fun main() {
    user_main()
}
