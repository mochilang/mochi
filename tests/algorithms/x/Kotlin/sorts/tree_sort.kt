fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class Node(var value: Int = 0, var left: Int = 0, var right: Int = 0)
data class TreeState(var nodes: MutableList<Node> = mutableListOf<Node>(), var root: Int = 0)
fun new_node(state: TreeState, value: Int): Int {
    state.nodes = run { val _tmp = (state.nodes).toMutableList(); _tmp.add(Node(value = value, left = 0 - 1, right = 0 - 1)); _tmp }
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
        if (value < node.value) {
            if (node.left == (0 - 1)) {
                var idx: Int = (new_node(state, value)).toInt()
                nodes = state.nodes
                node.left = idx
                _listSet(nodes, current, node)
                state.nodes = nodes
                return
            }
            current = node.left
        } else {
            if (value > node.value) {
                if (node.right == (0 - 1)) {
                    var idx: Int = (new_node(state, value)).toInt()
                    nodes = state.nodes
                    node.right = idx
                    _listSet(nodes, current, node)
                    state.nodes = nodes
                    return
                }
                current = node.right
            } else {
                return
            }
        }
    }
}

fun inorder(state: TreeState, idx: Int): MutableList<Int> {
    if (idx == (0 - 1)) {
        return mutableListOf<Int>()
    }
    var node: Node = (state.nodes)[idx]!!
    var result: MutableList<Int> = inorder(state, node.left)
    result = run { val _tmp = result.toMutableList(); _tmp.add(node.value); _tmp }
    var right_part: MutableList<Int> = inorder(state, node.right)
    var i: Int = (0).toInt()
    while (i < right_part.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right_part[i]!!); _tmp }
        i = i + 1
    }
    return result
}

fun tree_sort(arr: MutableList<Int>): MutableList<Int> {
    var state: TreeState = TreeState(nodes = mutableListOf<Node>(), root = 0 - 1)
    var i: Int = (0).toInt()
    while (i < arr.size) {
        insert(state, arr[i]!!)
        i = i + 1
    }
    if (state.root == (0 - 1)) {
        return mutableListOf<Int>()
    }
    return inorder(state, state.root)
}

fun main() {
    println(tree_sort(mutableListOf<Int>()).toString())
    println(tree_sort(mutableListOf(1)).toString())
    println(tree_sort(mutableListOf(1, 2)).toString())
    println(tree_sort(mutableListOf(5, 2, 7)).toString())
    println(tree_sort(mutableListOf(5, 0 - 4, 9, 2, 7)).toString())
    println(tree_sort(mutableListOf(5, 6, 1, 0 - 1, 4, 37, 2, 7)).toString())
}
