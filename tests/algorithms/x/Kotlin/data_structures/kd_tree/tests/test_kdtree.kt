val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/kd_tree/tests"

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

var INF: Double = 1000000000.0
var seed: Int = (1).toInt()
fun rand_float(): Double {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return (seed.toDouble()) / 2147483648.0
}

fun hypercube_points(num_points: Int, cube_size: Double, num_dimensions: Int): MutableList<MutableList<Double>> {
    var pts: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < num_points) {
        var p: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < num_dimensions) {
            var v: Double = cube_size * rand_float()
            p = run { val _tmp = p.toMutableList(); _tmp.add(v); _tmp }
            j = j + 1
        }
        pts = run { val _tmp = pts.toMutableList(); _tmp.add(p); _tmp }
        i = i + 1
    }
    return pts
}

fun build_kdtree(points: MutableList<MutableList<Double>>, depth: Int): MutableList<MutableList<Double>> {
    return points
}

fun distance_sq(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        var d: Double = a[i]!! - b[i]!!
        sum = sum + (d * d)
        i = i + 1
    }
    return sum
}

fun nearest_neighbour_search(points: MutableList<MutableList<Double>>, query: MutableList<Double>): MutableMap<String, Double> {
    if (points.size == 0) {
        return mutableMapOf<String, Double>("index" to (0.0 - 1.0), "dist" to (INF), "visited" to (0.0))
    }
    var nearest_idx: Int = (0).toInt()
    var nearest_dist: Double = INF
    var visited: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < points.size) {
        var d: Double = distance_sq(query, points[i]!!)
        visited = visited + 1
        if (d < nearest_dist) {
            nearest_dist = d
            nearest_idx = i
        }
        i = i + 1
    }
    return mutableMapOf<String, Double>("index" to (nearest_idx.toDouble()), "dist" to (nearest_dist), "visited" to (visited.toDouble()))
}

fun test_build_cases(): Unit {
    var empty_pts: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var tree0: MutableList<MutableList<Double>> = build_kdtree(empty_pts, 0)
    if (tree0.size == 0) {
        println("case1 true")
    } else {
        println("case1 false")
    }
    var pts1: MutableList<MutableList<Double>> = hypercube_points(10, 10.0, 2)
    var tree1: MutableList<MutableList<Double>> = build_kdtree(pts1, 2)
    if ((tree1.size > 0) && ((tree1[0]!!).size == 2)) {
        println("case2 true")
    } else {
        println("case2 false")
    }
    var pts2: MutableList<MutableList<Double>> = hypercube_points(10, 10.0, 3)
    var tree2: MutableList<MutableList<Double>> = build_kdtree(pts2, 0 - 2)
    if ((tree2.size > 0) && ((tree2[0]!!).size == 3)) {
        println("case3 true")
    } else {
        println("case3 false")
    }
}

fun test_search(): Unit {
    var pts: MutableList<MutableList<Double>> = hypercube_points(10, 10.0, 2)
    var tree: MutableList<MutableList<Double>> = build_kdtree(pts, 0)
    var qp: MutableList<Double> = (hypercube_points(1, 10.0, 2))[0]!!
    var res: MutableMap<String, Double> = nearest_neighbour_search(tree, qp)
    if (((((res)["index"] as Double != (0.0 - 1.0)) && ((res)["dist"] as Double >= 0.0) as Boolean)) && ((res)["visited"] as Double > 0.0)) {
        println("search true")
    } else {
        println("search false")
    }
}

fun test_edge(): Unit {
    var empty_pts: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var tree: MutableList<MutableList<Double>> = build_kdtree(empty_pts, 0)
    var query: MutableList<Double> = mutableListOf(0.0, 0.0)
    var res: MutableMap<String, Double> = nearest_neighbour_search(tree, query)
    if (((((res)["index"] as Double == (0.0 - 1.0)) && ((res)["dist"] as Double > 100000000.0) as Boolean)) && ((res)["visited"] as Double == 0.0)) {
        println("edge true")
    } else {
        println("edge false")
    }
}

fun user_main(): Unit {
    seed = (1).toInt()
    test_build_cases()
    test_search()
    test_edge()
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
