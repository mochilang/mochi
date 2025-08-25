val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/kd_tree/example"

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

var seed: Int = (1).toInt()
var pts: MutableList<MutableList<Double>> = hypercube_points(3, 1.0, 2)
fun rand(): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return seed
}

fun random(): Double {
    return ((rand()).toDouble()) / 2147483648.0
}

fun hypercube_points(num_points: Int, hypercube_size: Double, num_dimensions: Int): MutableList<MutableList<Double>> {
    var points: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < num_points) {
        var point: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < num_dimensions) {
            var value: Double = hypercube_size * random()
            point = run { val _tmp = point.toMutableList(); _tmp.add(value); _tmp }
            j = j + 1
        }
        points = run { val _tmp = points.toMutableList(); _tmp.add(point); _tmp }
        i = i + 1
    }
    return points
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(pts)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
