fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

data class Node(var key: Int = 0, var left: Int = 0, var right: Int = 0)
var tree: MutableList<Node> = mutableListOf(Node(key = 10, left = 1, right = 2), Node(key = 5, left = 3, right = 4), Node(key = 20, left = 5, right = 6), Node(key = 3, left = 0 - 1, right = 0 - 1), Node(key = 7, left = 0 - 1, right = 0 - 1), Node(key = 15, left = 0 - 1, right = 0 - 1), Node(key = 25, left = 0 - 1, right = 0 - 1))
fun inorder(nodes: MutableList<Node>, idx: Int): MutableList<Int> {
    if (idx == (0 - 1)) {
        return mutableListOf<Int>()
    }
    var node: Node = nodes[idx]!!
    var result: MutableList<Int> = ((inorder(nodes, node.left)) as MutableList<Int>)
    result = run { val _tmp = result.toMutableList(); _tmp.add(node.key); _tmp }
    result = ((concat(result, inorder(nodes, node.right))) as MutableList<Int>)
    return result
}

fun floor_ceiling(nodes: MutableList<Node>, idx: Int, key: Int): MutableList<Int> {
    var floor_val: Any? = null
    var ceiling_val: Any? = null
    var current: Int = (idx).toInt()
    while (current != (0 - 1)) {
        var node: Node = nodes[current]!!
        if (node.key == key) {
            floor_val = ((node.key) as Any?)
            ceiling_val = ((node.key) as Any?)
            break
        }
        if (key < node.key) {
            ceiling_val = ((node.key) as Any?)
            current = node.left
        } else {
            floor_val = ((node.key) as Any?)
            current = node.right
        }
    }
    return mutableListOf<Int>((floor_val as Int), (ceiling_val as Int))
}

fun main() {
    println(inorder(tree, 0).toString())
    println(floor_ceiling(tree, 0, 8).toString())
    println(floor_ceiling(tree, 0, 14).toString())
    println(floor_ceiling(tree, 0, 0 - 1).toString())
    println(floor_ceiling(tree, 0, 30).toString())
}
