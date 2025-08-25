import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/kd_tree/example"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

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

data class Node(var point: MutableList<Double> = mutableListOf<Double>(), var left: Int = 0, var right: Int = 0)
data class BuildResult(var index: Int = 0, var nodes: MutableList<Node> = mutableListOf<Node>())
data class SearchResult(var point: MutableList<Double> = mutableListOf<Double>(), var dist: Double = 0.0, var visited: Int = 0)
var seed: Int = (1).toInt()
fun rand(): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return seed
}

fun random(): Double {
    return (1.0 * (rand()).toDouble()) / 2147483648.0
}

fun hypercube_points(num_points: Int, cube_size: Double, num_dimensions: Int): MutableList<MutableList<Double>> {
    var pts: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < num_points) {
        var p: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < num_dimensions) {
            p = run { val _tmp = p.toMutableList(); _tmp.add(cube_size * random()); _tmp }
            j = j + 1
        }
        pts = run { val _tmp = pts.toMutableList(); _tmp.add(p); _tmp }
        i = i + 1
    }
    return pts
}

fun sort_points(points: MutableList<MutableList<Double>>, axis: Int): MutableList<MutableList<Double>> {
    var n: Int = (points.size).toInt()
    var i: Int = (1).toInt()
    while (i < n) {
        var key: MutableList<Double> = points[i]!!
        var j: Int = (i - 1).toInt()
        while ((j >= 0) && (((points[j]!!) as MutableList<Double>)[axis]!! > key[axis]!!)) {
            _listSet(points, j + 1, points[j]!!)
            j = j - 1
        }
        _listSet(points, j + 1, key)
        i = i + 1
    }
    return points
}

fun sublist(arr: MutableList<MutableList<Double>>, start: Int, end: Int): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (start).toInt()
    while (i < end) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(arr[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun shift_nodes(nodes: MutableList<Node>, offset: Int): MutableList<Node> {
    var i: Int = (0).toInt()
    while (i < nodes.size) {
        if (nodes[i]!!.left != (0 - 1)) {
            nodes[i]!!.left = nodes[i]!!.left + offset
        }
        if (nodes[i]!!.right != (0 - 1)) {
            nodes[i]!!.right = nodes[i]!!.right + offset
        }
        i = i + 1
    }
    return nodes
}

fun build_kdtree(points: MutableList<MutableList<Double>>, depth: Int): BuildResult {
    var points: MutableList<MutableList<Double>> = points
    if (points.size == 0) {
        return BuildResult(index = 0 - 1, nodes = mutableListOf<Node>())
    }
    var k: Int = ((points[0]!!).size).toInt()
    var axis: Int = (Math.floorMod(depth, k)).toInt()
    points = sort_points(points, axis)
    var median: Int = (Math.floorDiv(points.size, 2)).toInt()
    var left_points: MutableList<MutableList<Double>> = sublist(points, 0, median)
    var right_points: MutableList<MutableList<Double>> = sublist(points, median + 1, points.size)
    var left_res: BuildResult = build_kdtree(left_points, depth + 1)
    var right_res: BuildResult = build_kdtree(right_points, depth + 1)
    var offset: Int = ((left_res.nodes).size + 1).toInt()
    var shifted_right: MutableList<Node> = shift_nodes(right_res.nodes, offset)
    var nodes: MutableList<Node> = left_res.nodes
    var left_index: Int = (left_res.index).toInt()
    var right_index = if (right_res.index == (0 - 1)) 0 - 1 else right_res.index + offset
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(point = points[median]!!, left = left_index, right = right_index.toInt())); _tmp }
    nodes = concat(nodes, shifted_right)
    var root_index: Int = ((left_res.nodes).size).toInt()
    return BuildResult(index = root_index, nodes = nodes)
}

fun square_distance(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        var diff: Double = a[i]!! - b[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sum
}

fun nearest_neighbour_search(tree: MutableList<Node>, root: Int, query_point: MutableList<Double>): SearchResult {
    var nearest_point: MutableList<Double> = mutableListOf<Double>()
    var nearest_dist: Double = 0.0
    var visited: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < tree.size) {
        var node: Node = tree[i]!!
        var dist: Double = square_distance(query_point, node.point)
        visited = visited + 1
        if ((visited == 1) || (dist < nearest_dist)) {
            nearest_point = node.point
            nearest_dist = dist
        }
        i = i + 1
    }
    return SearchResult(point = nearest_point, dist = nearest_dist, visited = visited)
}

fun list_to_string(arr: MutableList<Double>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < arr.size) {
        s = s + _numToStr(arr[i]!!)
        if (i < (arr.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    return s + "]"
}

fun user_main(): Unit {
    var num_points: Int = (5000).toInt()
    var cube_size: Double = 10.0
    var num_dimensions: Int = (10).toInt()
    var pts: MutableList<MutableList<Double>> = hypercube_points(num_points, cube_size, num_dimensions)
    var build: BuildResult = build_kdtree(pts, 0)
    var root: Int = (build.index).toInt()
    var tree: MutableList<Node> = build.nodes
    var query: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < num_dimensions) {
        query = run { val _tmp = query.toMutableList(); _tmp.add(random()); _tmp }
        i = i + 1
    }
    var res: SearchResult = nearest_neighbour_search(tree, root, query)
    println("Query point: " + list_to_string(query))
    println("Nearest point: " + list_to_string(res.point))
    println("Distance: " + _numToStr(res.dist))
    println("Nodes visited: " + _numToStr(res.visited))
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
