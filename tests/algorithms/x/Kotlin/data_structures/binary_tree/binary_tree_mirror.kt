fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun binary_tree_mirror_dict(tree: MutableMap<Int, MutableList<Int>>, root: Int): Unit {
    if ((root == 0) || (!(root in tree) as Boolean)) {
        return
    }
    var children: MutableList<Int> = (tree)[root] as MutableList<Int>
    var left: Int = (children[0]!!).toInt()
    var right: Int = (children[1]!!).toInt()
    (tree)[root] = mutableListOf(right, left)
    binary_tree_mirror_dict(tree, left)
    binary_tree_mirror_dict(tree, right)
}

fun binary_tree_mirror(binary_tree: MutableMap<Int, MutableList<Int>>, root: Int): MutableMap<Int, MutableList<Int>> {
    if (binary_tree.size == 0) {
        panic("binary tree cannot be empty")
    }
    if (!(root in binary_tree)) {
        panic(("root " + root.toString()) + " is not present in the binary_tree")
    }
    var tree_copy: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>()
    for (k in binary_tree.keys) {
        (tree_copy)[k] = (binary_tree)[k] as MutableList<Int>
    }
    binary_tree_mirror_dict(tree_copy, root)
    return tree_copy
}

fun user_main(): Unit {
    var binary_tree: MutableMap<Int, MutableList<Int>> = (mutableMapOf<Int, MutableList<Int>>(1 to (mutableListOf(2, 3)), 2 to (mutableListOf(4, 5)), 3 to (mutableListOf(6, 7)), 7 to (mutableListOf(8, 9))) as MutableMap<Int, MutableList<Int>>)
    println("Binary tree: " + binary_tree.toString())
    var mirrored: MutableMap<Int, MutableList<Int>> = binary_tree_mirror(binary_tree, 1)
    println("Binary tree mirror: " + mirrored.toString())
}

fun main() {
    user_main()
}
