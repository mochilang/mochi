import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/kd_tree"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
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
var tree: MutableList<KDNode> = mutableListOf<KDNode>()
var pts: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 3.0), mutableListOf(5.0, 4.0), mutableListOf(9.0, 6.0), mutableListOf(4.0, 7.0), mutableListOf(8.0, 1.0), mutableListOf(7.0, 2.0))
var root: Int = (build_kdtree(pts, 0)).toInt()
fun sort_points(points: MutableList<MutableList<Double>>, axis: Int): MutableList<MutableList<Double>> {
    var arr: MutableList<MutableList<Double>> = points
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var j: Int = (0).toInt()
        while (j < (arr.size - 1)) {
            if (((arr[j]!!) as MutableList<Double>)[axis]!! > ((arr[j + 1]!!) as MutableList<Double>)[axis]!!) {
                var tmp: MutableList<Double> = arr[j]!!
                _listSet(arr, j, arr[j + 1]!!)
                _listSet(arr, j + 1, tmp)
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun build_kdtree(points: MutableList<MutableList<Double>>, depth: Int): Int {
    if (points.size == 0) {
        return 0 - 1
    }
    var k: Int = ((points[0]!!).size).toInt()
    var axis: Int = (Math.floorMod(depth, k)).toInt()
    var sorted: MutableList<MutableList<Double>> = sort_points(points, axis)
    var median_idx: Int = (Math.floorDiv(sorted.size, 2)).toInt()
    var left_points: MutableList<MutableList<Double>> = _sliceList(sorted, 0, median_idx)
    var right_points: MutableList<MutableList<Double>> = _sliceList(sorted, median_idx + 1, sorted.size)
    var idx: Int = (tree.size).toInt()
    tree = run { val _tmp = tree.toMutableList(); _tmp.add(KDNode(point = sorted[median_idx]!!, left = 0 - 1, right = 0 - 1)); _tmp }
    var left_idx: Int = (build_kdtree(left_points, depth + 1)).toInt()
    var right_idx: Int = (build_kdtree(right_points, depth + 1)).toInt()
    var node: KDNode = tree[idx]!!
    node.left = left_idx
    node.right = right_idx
    _listSet(tree, idx, node)
    return idx
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(tree.toString())
        println(root)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
