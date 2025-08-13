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

data class Vec(var x: Double = 0.0, var y: Double = 0.0)
var PI: Double = 3.141592653589793
var TWO_PI: Double = 6.283185307179586
var VECTOR_1: Vec = Vec(x = 0.0, y = 0.0)
var VECTOR_2: Vec = Vec(x = 0.5, y = 0.8660254)
var VECTOR_3: Vec = Vec(x = 1.0, y = 0.0)
var INITIAL_VECTORS: MutableList<Vec> = mutableListOf(VECTOR_1, VECTOR_2, VECTOR_3, VECTOR_1)
var example: MutableList<Vec> = iterate(mutableListOf(VECTOR_1, VECTOR_3), 1)
fun _mod(x: Double, m: Double): Double {
    return x - ((((((x / m).toInt())).toDouble())) * m)
}

fun sin(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y3: Double = y2 * y
    var y5: Double = y3 * y2
    var y7: Double = y5 * y2
    return ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
}

fun cos(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y4: Double = y2 * y2
    var y6: Double = y4 * y2
    return ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
}

fun rotate(v: Vec, angle_deg: Double): Vec {
    var theta: Double = (angle_deg * PI) / 180.0
    var c: Double = cos(theta)
    var s: Double = sin(theta)
    return Vec(x = (v.x * c) - (v.y * s), y = (v.x * s) + (v.y * c))
}

fun iteration_step(vectors: MutableList<Vec>): MutableList<Vec> {
    var new_vectors: MutableList<Vec> = mutableListOf<Vec>()
    var i: Int = (0).toInt()
    while (i < (vectors.size - 1)) {
        var start: Vec = vectors[i]!!
        var end: Vec = vectors[i + 1]!!
        new_vectors = run { val _tmp = new_vectors.toMutableList(); _tmp.add(start); _tmp }
        var dx: Double = end.x - start.x
        var dy: Double = end.y - start.y
        var one_third: Vec = Vec(x = start.x + (dx / 3.0), y = start.y + (dy / 3.0))
        var mid: Vec = rotate(Vec(x = dx / 3.0, y = dy / 3.0), 60.0)
        var peak: Vec = Vec(x = one_third.x + mid.x, y = one_third.y + mid.y)
        var two_third: Vec = Vec(x = start.x + ((dx * 2.0) / 3.0), y = start.y + ((dy * 2.0) / 3.0))
        new_vectors = run { val _tmp = new_vectors.toMutableList(); _tmp.add(one_third); _tmp }
        new_vectors = run { val _tmp = new_vectors.toMutableList(); _tmp.add(peak); _tmp }
        new_vectors = run { val _tmp = new_vectors.toMutableList(); _tmp.add(two_third); _tmp }
        i = i + 1
    }
    new_vectors = run { val _tmp = new_vectors.toMutableList(); _tmp.add(vectors[vectors.size - 1]!!); _tmp }
    return new_vectors
}

fun iterate(initial: MutableList<Vec>, steps: Int): MutableList<Vec> {
    var vectors: MutableList<Vec> = initial
    var i: Int = (0).toInt()
    while (i < steps) {
        vectors = iteration_step(vectors)
        i = i + 1
    }
    return vectors
}

fun vec_to_string(v: Vec): String {
    return ((("(" + v.x.toString()) + ", ") + v.y.toString()) + ")"
}

fun vec_list_to_string(lst: MutableList<Vec>): String {
    var res: String = "["
    var i: Int = (0).toInt()
    while (i < lst.size) {
        res = res + vec_to_string(lst[i]!!)
        if (i < (lst.size - 1)) {
            res = res + ", "
        }
        i = i + 1
    }
    res = res + "]"
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(vec_list_to_string(example))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
