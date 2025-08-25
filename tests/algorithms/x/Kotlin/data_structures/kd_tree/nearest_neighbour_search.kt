import java.math.BigInteger

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
data class SearchResult(var point: MutableList<Double> = mutableListOf<Double>(), var distance: Double = 0.0, var nodes_visited: Int = 0)
var nodes: MutableList<KDNode> = mutableListOf(KDNode(point = mutableListOf(9.0, 1.0), left = 1, right = 4), KDNode(point = mutableListOf(2.0, 7.0), left = 2, right = 3), KDNode(point = mutableListOf(3.0, 6.0), left = 0 - 1, right = 0 - 1), KDNode(point = mutableListOf(6.0, 12.0), left = 0 - 1, right = 0 - 1), KDNode(point = mutableListOf(17.0, 15.0), left = 5, right = 6), KDNode(point = mutableListOf(13.0, 15.0), left = 0 - 1, right = 0 - 1), KDNode(point = mutableListOf(10.0, 19.0), left = 0 - 1, right = 0 - 1))
var queries: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(9.0, 2.0), mutableListOf(12.0, 15.0), mutableListOf(1.0, 3.0))
var q: Int = (0).toInt()
fun square_distance(a: MutableList<Double>, b: MutableList<Double>): Double {
    var i: Int = (0).toInt()
    var total: Double = 0.0
    while (i < a.size) {
        var diff: Double = a[i]!! - b[i]!!
        total = total + (diff * diff)
        i = i + 1
    }
    return total
}

fun search(nodes: MutableList<KDNode>, index: Int, query_point: MutableList<Double>, depth: Int, best: SearchResult): SearchResult {
    if (index == (0 - 1)) {
        return best
    }
    var result: SearchResult = best
    result.nodes_visited = result.nodes_visited + 1
    var node: KDNode = nodes[index]!!
    var current_point: MutableList<Double> = node.point
    var current_dist: Double = square_distance(query_point, current_point)
    if (((result.point).size == 0) || (current_dist < result.distance)) {
        result.point = current_point
        result.distance = current_dist
    }
    var k: Int = (query_point.size).toInt()
    var axis: Int = (Math.floorMod(depth, k)).toInt()
    var nearer: Int = (node.left).toInt()
    var further: Int = (node.right).toInt()
    if (query_point[axis]!! > current_point[axis]!!) {
        nearer = node.right
        further = node.left
    }
    result = search(nodes, nearer, query_point, depth + 1, result)
    var diff: Double = query_point[axis]!! - current_point[axis]!!
    if ((diff * diff) < result.distance) {
        result = search(nodes, further, query_point, depth + 1, result)
    }
    return result
}

fun nearest_neighbour_search(nodes: MutableList<KDNode>, root: Int, query_point: MutableList<Double>): SearchResult {
    var initial: SearchResult = SearchResult(point = mutableListOf<Double>(), distance = 1000000000000000000000000000000.0, nodes_visited = 0)
    return search(nodes, root, query_point, 0, initial)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (q < queries.size) {
            var res: SearchResult = nearest_neighbour_search(nodes, 0, queries[q]!!)
            println(((((res.point.toString() + " ") + _numToStr(res.distance)) + " ") + _numToStr(res.nodes_visited)) + "\n")
            q = (q + 1).toInt()
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
