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

fun multiplier(n1: Double, n2: Double): (Double) -> Double {
    var n1n2: Double = n1 * n2
    return { m: Double -> n1n2 * m } as (Double) -> Double
}

fun user_main(): Unit {
    var x: Double = 2.0
    var xi: Double = 0.5
    var y: Double = 4.0
    var yi: Double = 0.25
    var z: Double = x + y
    var zi: Double = 1.0 / (x + y)
    var numbers: MutableList<Double> = mutableListOf(x, y, z)
    var inverses: MutableList<Double> = mutableListOf(xi, yi, zi)
    var mfs: MutableList<(Double) -> Double> = mutableListOf<(Double) -> Double>()
    var i: Int = 0
    while (i < numbers.size) {
        mfs = run { val _tmp = mfs.toMutableList(); _tmp.add(multiplier(numbers[i]!!, inverses[i]!!)); _tmp }
        i = i + 1
    }
    for (mf in mfs) {
        println(mf(1.0).toString())
    }
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
