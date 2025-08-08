fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

data class Node(var data: Int = 0, var left: Int = 0, var right: Int = 0)
data class TreeState(var nodes: MutableList<Node> = mutableListOf<Node>(), var root: Int = 0)
fun new_node(state: TreeState, value: Int): Int {
    state.nodes = run { val _tmp = (state.nodes).toMutableList(); _tmp.add(Node(data = value, left = 0 - 1, right = 0 - 1)); _tmp }
    return (state.nodes).size - 1
}

fun insert(state: TreeState, value: Int): Unit {
    if (state.root == (0 - 1)) {
        state.root = new_node(state, value)
        return
    }
    var current: Int = (state.root).toInt()
    var nodes: MutableList<Node> = state.nodes
    while (true) {
        var node: Node = nodes[current]!!
        if (value < node.data) {
            if (node.left == (0 - 1)) {
                node.left = new_node(state, value)
                _listSet(nodes, current, node)
                state.nodes = nodes
                return
            }
            current = node.left
        } else {
            if (node.right == (0 - 1)) {
                node.right = new_node(state, value)
                _listSet(nodes, current, node)
                state.nodes = nodes
                return
            }
            current = node.right
        }
    }
}

fun inorder(state: TreeState, idx: Int): MutableList<Int> {
    if (idx == (0 - 1)) {
        return mutableListOf<Int>()
    }
    var node: Node = (state.nodes)[idx]!!
    var result: MutableList<Int> = ((inorder(state, node.left)) as MutableList<Int>)
    result = run { val _tmp = result.toMutableList(); _tmp.add(node.data); _tmp }
    var right_part: MutableList<Int> = inorder(state, node.right)
    var i: Int = (0).toInt()
    while (i < right_part.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right_part[i]!!); _tmp }
        i = i + 1
    }
    return result
}

fun make_tree(): TreeState {
    var state: TreeState = TreeState(nodes = mutableListOf<Node>(), root = 0 - 1)
    insert(state, 15)
    insert(state, 10)
    insert(state, 25)
    insert(state, 6)
    insert(state, 14)
    insert(state, 20)
    insert(state, 60)
    return state
}

fun user_main(): Unit {
    var state: TreeState = make_tree()
    println("Printing values of binary search tree in Inorder Traversal.")
    println(inorder(state, state.root))
}

fun test_inorder_traversal(): Unit {
    var state: TreeState = make_tree()
    expect(inorder(state, state.root) == mutableListOf(6, 10, 14, 15, 20, 25, 60))
}

fun main() {
    user_main()
    test_inorder_traversal()
}
