var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

val PI: Double = 3.141592653589793
val dt: Double = 0.01
var s: Double = 0.0
var t1: Double = 0.0
var k1: Double = sinApprox(0.0)
var i: Int = 1
var i2: Int = 1
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 12) {
        val denom: Double = ((2 * n) * ((2 * n) + 1)).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i <= 200) {
            val t2: Double = i.toDouble() * dt
            val k2: Double = sinApprox(t2 * PI)
            s = s + (((k1 + k2) * 0.5) * (t2 - t1))
            t1 = t2
            k1 = k2
            i = i + 1
        }
        while (i2 <= 50) {
            val t2: Double = 2.0 + (i2.toDouble() * dt)
            val k2: Double = 0.0
            s = s + (((k1 + k2) * 0.5) * (t2 - t1))
            t1 = t2
            k1 = k2
            i2 = i2 + 1
        }
        println(s)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
