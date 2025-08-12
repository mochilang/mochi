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

fun check_polygon(nums: MutableList<Double>): Boolean {
    if (nums.size < 2) {
        error("Monogons and Digons are not polygons in the Euclidean space")
    }
    var i: Int = (0).toInt()
    while (i < nums.size) {
        if (nums[i]!! <= 0.0) {
            error("All values must be greater than 0")
        }
        i = i + 1
    }
    var total: Double = 0.0
    var max_side: Double = 0.0
    i = 0
    while (i < nums.size) {
        var v: Double = nums[i]!!
        total = total + v
        if (v > max_side) {
            max_side = v
        }
        i = i + 1
    }
    return max_side < (total - max_side)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(check_polygon(mutableListOf(6.0, 10.0, 5.0)).toString())
        println(check_polygon(mutableListOf(3.0, 7.0, 13.0, 2.0)).toString())
        println(check_polygon(mutableListOf(1.0, 4.3, 5.2, 12.2)).toString())
        var nums: MutableList<Double> = mutableListOf(3.0, 7.0, 13.0, 2.0)
        var _u1: Boolean = check_polygon(nums)
        println(nums.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
