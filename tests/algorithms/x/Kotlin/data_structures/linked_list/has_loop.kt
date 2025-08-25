val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

data class Node(var data: Int = 0, var next: Int = 0)
fun has_loop(nodes: MutableList<Node>, head: Int): Boolean {
    var slow: Int = (head).toInt()
    var fast: Int = (head).toInt()
    while (fast != (0 - 1)) {
        var fast_node1: Node = nodes[fast]!!
        if (fast_node1.next == (0 - 1)) {
            return false
        }
        var fast_node2: Node = nodes[fast_node1.next]!!
        if (fast_node2.next == (0 - 1)) {
            return false
        }
        var slow_node: Node = nodes[slow]!!
        slow = slow_node.next
        fast = fast_node2.next
        if (slow == fast) {
            return true
        }
    }
    return false
}

fun make_nodes(values: MutableList<Int>): MutableList<Node> {
    var nodes: MutableList<Node> = mutableListOf<Node>()
    var i: Int = (0).toInt()
    while (i < values.size) {
        var next_idx = if (i == (values.size - 1)) 0 - 1 else i + 1
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = values[i]!!, next = next_idx)); _tmp }
        i = i + 1
    }
    return nodes
}

fun user_main(): Unit {
    var list1: MutableList<Node> = make_nodes(mutableListOf(1, 2, 3, 4))
    println(has_loop(list1, 0).toString())
    list1[3]!!.next = 1
    println(has_loop(list1, 0).toString())
    var list2: MutableList<Node> = make_nodes(mutableListOf(5, 6, 5, 6))
    println(has_loop(list2, 0).toString())
    var list3: MutableList<Node> = make_nodes(mutableListOf(1))
    println(has_loop(list3, 0).toString())
}

fun main() {
    user_main()
}
