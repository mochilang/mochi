fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var node_data: MutableList<Int> = mutableListOf<Int>(0)
var left_child: MutableList<Int> = mutableListOf<Int>(0)
var right_child: MutableList<Int> = mutableListOf<Int>(0)
fun new_node(value: Int): Int {
    node_data = run { val _tmp = node_data.toMutableList(); _tmp.add(value); _tmp }
    left_child = run { val _tmp = left_child.toMutableList(); _tmp.add(0); _tmp }
    right_child = run { val _tmp = right_child.toMutableList(); _tmp.add(0); _tmp }
    return node_data.size - 1
}

fun build_tree(): Int {
    var root: Int = (new_node(1)).toInt()
    var n2: Int = (new_node(2)).toInt()
    var n5: Int = (new_node(5)).toInt()
    var n3: Int = (new_node(3)).toInt()
    var n4: Int = (new_node(4)).toInt()
    var n6: Int = (new_node(6)).toInt()
    _listSet(left_child, root, n2)
    _listSet(right_child, root, n5)
    _listSet(left_child, n2, n3)
    _listSet(right_child, n2, n4)
    _listSet(right_child, n5, n6)
    return root
}

fun flatten(root: Int): MutableList<Int> {
    if (root == 0) {
        return mutableListOf<Int>()
    }
    var res: MutableList<Int> = mutableListOf<Int>(node_data[root]!!)
    var left_vals: MutableList<Int> = flatten(left_child[root]!!)
    var right_vals: MutableList<Int> = flatten(right_child[root]!!)
    var i: Int = (0).toInt()
    while (i < left_vals.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(left_vals[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < right_vals.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(right_vals[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun display(values: MutableList<Int>): Unit {
    var s: String = ""
    var i: Int = (0).toInt()
    while (i < values.size) {
        if (i == 0) {
            s = (values[i]!!).toString()
        } else {
            s = (s + " ") + (values[i]!!).toString()
        }
        i = i + 1
    }
    println(s)
}

fun main() {
    println("Flattened Linked List:")
    var root: Int = (build_tree()).toInt()
    var vals: MutableList<Int> = flatten(root)
    display(vals)
}
