val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/binary_tree"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

fun binary_tree_mirror_dict(tree: MutableMap<Int, MutableList<Int>>, root: Int): Unit {
    if ((root == 0) || (!(tree.containsKey(root)) as Boolean)) {
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
    if (!(binary_tree.containsKey(root))) {
        panic(("root " + _numToStr(root)) + " is not present in the binary_tree")
    }
    var tree_copy: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>()
    for (k in binary_tree.keys) {
        (tree_copy)[k] = (binary_tree)[k] as MutableList<Int>
    }
    binary_tree_mirror_dict(tree_copy, root)
    return tree_copy
}

fun user_main(): Unit {
    var binary_tree: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>(1 to (mutableListOf(2, 3)), 2 to (mutableListOf(4, 5)), 3 to (mutableListOf(6, 7)), 7 to (mutableListOf(8, 9))) as MutableMap<Int, MutableList<Int>>
    println("Binary tree: " + binary_tree.toString())
    var mirrored: MutableMap<Int, MutableList<Int>> = binary_tree_mirror(binary_tree, 1)
    println("Binary tree mirror: " + mirrored.toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
