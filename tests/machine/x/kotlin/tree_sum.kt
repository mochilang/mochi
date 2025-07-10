fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

val t = Node(left = Leaf, value = 1, right = Node(left = Leaf, value = 2, right = Leaf))

fun sum_tree(t: Tree): Int {
    return run {
    val __t = t
    when (__t) {
        Leaf -> 0
        Node(left, value, right) -> toInt(sum_tree(left) + toInt(value)) + sum_tree(right)
    }
}
}

fun main() {
    println(sum_tree(t))
}
