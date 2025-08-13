import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class Node(var key: Int = 0, var freq: Int = 0)
fun sort_nodes(nodes: MutableList<Node>): MutableList<Node> {
    var arr: MutableList<Node> = nodes
    var i: Int = (1).toInt()
    while (i < arr.size) {
        var key_node: Node = arr[i]!!
        var j: Int = (i - 1).toInt()
        while (j >= 0) {
            var temp: Node = arr[j]!!
            if (temp.key > key_node.key) {
                _listSet(arr, j + 1, temp)
                j = j - 1
            } else {
                break
            }
        }
        _listSet(arr, j + 1, key_node)
        i = i + 1
    }
    return arr
}

fun print_node(n: Node): Unit {
    println(((("Node(key=" + n.key.toString()) + ", freq=") + n.freq.toString()) + ")")
}

fun print_binary_search_tree(root: MutableList<MutableList<Int>>, keys: MutableList<Int>, i: Int, j: Int, parent: Int, is_left: Boolean): Unit {
    if ((((i > j) || (i < 0) as Boolean)) || (j > (root.size - 1))) {
        return
    }
    var node: Int = ((((root[i]!!) as MutableList<Int>))[j]!!).toInt()
    if (parent == (0 - 1)) {
        println((keys[node]!!).toString() + " is the root of the binary search tree.")
    } else {
        if ((is_left as Boolean)) {
            println((((keys[node]!!).toString() + " is the left child of key ") + parent.toString()) + ".")
        } else {
            println((((keys[node]!!).toString() + " is the right child of key ") + parent.toString()) + ".")
        }
    }
    print_binary_search_tree(root, keys, i, node - 1, keys[node]!!, true)
    print_binary_search_tree(root, keys, node + 1, j, keys[node]!!, false)
}

fun find_optimal_binary_search_tree(original_nodes: MutableList<Node>): Unit {
    var nodes: MutableList<Node> = sort_nodes(original_nodes)
    var n: Int = (nodes.size).toInt()
    var keys: MutableList<Int> = mutableListOf<Int>()
    var freqs: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        var node: Node = nodes[i]!!
        keys = run { val _tmp = keys.toMutableList(); _tmp.add(node.key); _tmp }
        freqs = run { val _tmp = freqs.toMutableList(); _tmp.add(node.freq); _tmp }
        i = i + 1
    }
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var total: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var root: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    i = 0
    while (i < n) {
        var dp_row: MutableList<Int> = mutableListOf<Int>()
        var total_row: MutableList<Int> = mutableListOf<Int>()
        var root_row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            if (i == j) {
                dp_row = run { val _tmp = dp_row.toMutableList(); _tmp.add(freqs[i]!!); _tmp }
                total_row = run { val _tmp = total_row.toMutableList(); _tmp.add(freqs[i]!!); _tmp }
                root_row = run { val _tmp = root_row.toMutableList(); _tmp.add(i); _tmp }
            } else {
                dp_row = run { val _tmp = dp_row.toMutableList(); _tmp.add(0); _tmp }
                total_row = run { val _tmp = total_row.toMutableList(); _tmp.add(0); _tmp }
                root_row = run { val _tmp = root_row.toMutableList(); _tmp.add(0); _tmp }
            }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(dp_row); _tmp }
        total = run { val _tmp = total.toMutableList(); _tmp.add(total_row); _tmp }
        root = run { val _tmp = root.toMutableList(); _tmp.add(root_row); _tmp }
        i = i + 1
    }
    var interval_length: Int = (2).toInt()
    var INF: Int = (2147483647).toInt()
    while (interval_length <= n) {
        i = 0
        while (i < ((n - interval_length) + 1)) {
            var j: Int = ((i + interval_length) - 1).toInt()
            _listSet(dp[i]!!, j, INF)
            _listSet(total[i]!!, j, (((total[i]!!) as MutableList<Int>))[j - 1]!! + freqs[j]!!)
            var r: Int = ((((root[i]!!) as MutableList<Int>))[j - 1]!!).toInt()
            while (r <= (((root[i + 1]!!) as MutableList<Int>))[j]!!) {
                var left: Int = (if (r != i) (((dp[i]!!) as MutableList<Int>))[r - 1]!! else 0).toInt()
                var right: Int = (if (r != j) (((dp[r + 1]!!) as MutableList<Int>))[j]!! else 0).toInt()
                var cost: Int = ((left + (((total[i]!!) as MutableList<Int>))[j]!!) + right).toInt()
                if ((((dp[i]!!) as MutableList<Int>))[j]!! > cost) {
                    _listSet(dp[i]!!, j, cost)
                    _listSet(root[i]!!, j, r)
                }
                r = r + 1
            }
            i = i + 1
        }
        interval_length = interval_length + 1
    }
    println("Binary search tree nodes:")
    i = 0
    while (i < n) {
        print_node(nodes[i]!!)
        i = i + 1
    }
    println(("\nThe cost of optimal BST for given tree nodes is " + ((((dp[0]!!) as MutableList<Int>))[n - 1]!!).toString()) + ".")
    print_binary_search_tree(root, keys, 0, n - 1, 0 - 1, false)
}

fun user_main(): Unit {
    var nodes: MutableList<Node> = mutableListOf(Node(key = 12, freq = 8), Node(key = 10, freq = 34), Node(key = 20, freq = 50), Node(key = 42, freq = 3), Node(key = 25, freq = 40), Node(key = 37, freq = 30))
    find_optimal_binary_search_tree(nodes)
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
