val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/kd_tree"

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

data class KDNode(var point: MutableList<Double> = mutableListOf<Double>(), var left: Int = 0, var right: Int = 0)
var nodes: MutableList<KDNode> = mutableListOf<KDNode>()
fun make_kd_node(point: MutableList<Double>, left: Int, right: Int): KDNode {
    return KDNode(point = point, left = left, right = right)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(make_kd_node(mutableListOf(2.0, 3.0), 1, 2)); _tmp }
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(make_kd_node(mutableListOf(1.0, 5.0), 0 - 1, 0 - 1)); _tmp }
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(make_kd_node(mutableListOf(4.0, 2.0), 0 - 1, 0 - 1)); _tmp }
        var root: KDNode = nodes[0]!!
        var left_child: KDNode = nodes[1]!!
        var right_child: KDNode = nodes[2]!!
        println(root.point.toString())
        println(_numToStr(root.left))
        println(_numToStr(root.right))
        println(left_child.point.toString())
        println(right_child.point.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
