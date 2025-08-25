import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/heap"

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

var NIL: Int = (0 - 1).toInt()
var seed: Int = (1).toInt()
var nodes: MutableList<MutableMap<String, Int>> = mutableListOf<MutableMap<String, Int>>()
var root: Int = (NIL).toInt()
fun set_seed(s: Int): Unit {
    seed = (s).toInt()
}

fun randint(a: Int, b: Int): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return (Math.floorMod(seed, ((b - a) + 1))) + a
}

fun rand_bool(): Boolean {
    return randint(0, 1) == 1
}

fun new_heap(): Unit {
    nodes = mutableListOf<MutableMap<String, Int>>()
    root = (NIL.toInt()).toInt()
}

fun merge(r1: Int, r2: Int): Int {
    var r2: Int = (r2).toInt()
    var r1: Int = (r1).toInt()
    if ((r1).toBigInteger().compareTo((NIL)) == 0) {
        return r2
    }
    if ((r2).toBigInteger().compareTo((NIL)) == 0) {
        return r1
    }
    if (((nodes[r1]!!) as MutableMap<String, Int>)["value"] as Int > ((nodes[r2]!!) as MutableMap<String, Int>)["value"] as Int) {
        var tmp: Int = (r1).toInt()
        r1 = r2
        r2 = tmp
    }
    if ((rand_bool()) as Boolean) {
        var tmp: Int = (((nodes[r1]!!) as MutableMap<String, Int>)["left"] as Int).toInt()
        (nodes[r1]!!)["left"] = ((nodes[r1]!!) as MutableMap<String, Int>)["right"] as Int
        (nodes[r1]!!)["right"] = tmp
    }
    (nodes[r1]!!)["left"] = merge(((nodes[r1]!!) as MutableMap<String, Int>)["left"] as Int, r2)
    return r1
}

fun insert(value: Int): Unit {
    var node: MutableMap<String, Int> = mutableMapOf<String, Any?>("value" to (value), "left" to (NIL), "right" to (NIL)) as MutableMap<String, Int>
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(node); _tmp }
    var idx: Int = (nodes.size - 1).toInt()
    root = (merge(root, idx)).toInt()
}

fun top(): Int {
    if ((root).toBigInteger().compareTo((NIL)) == 0) {
        return 0
    }
    return ((nodes[root]!!) as MutableMap<String, Int>)["value"] as Int
}

fun pop(): Int {
    var result: Int = (top()).toInt()
    var l: Int = (((nodes[root]!!) as MutableMap<String, Int>)["left"] as Int).toInt()
    var r: Int = (((nodes[root]!!) as MutableMap<String, Int>)["right"] as Int).toInt()
    root = (merge(l, r)).toInt()
    return result
}

fun is_empty(): Boolean {
    return (root).toBigInteger().compareTo((NIL)) == 0
}

fun to_sorted_list(): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    while (!is_empty()) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(pop()); _tmp }
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        set_seed(1)
        new_heap()
        insert(2)
        insert(3)
        insert(1)
        insert(5)
        insert(1)
        insert(7)
        println(to_sorted_list())
        new_heap()
        insert(1)
        insert(0 - 1)
        insert(0)
        println(to_sorted_list())
        new_heap()
        insert(3)
        insert(1)
        insert(3)
        insert(7)
        println(pop())
        println(pop())
        println(pop())
        println(pop())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
