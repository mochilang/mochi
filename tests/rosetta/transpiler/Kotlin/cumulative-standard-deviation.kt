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

data class Rsdv(var n: Double = 0.0, var a: Double = 0.0, var q: Double = 0.0)
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var g: Double = x
    var i: Int = 0
    while (i < 20) {
        g = (g + (x / g)) / 2.0
        i = i + 1
    }
    return g
}

fun newRsdv(): Rsdv {
    return Rsdv(n = 0.0, a = 0.0, q = 0.0)
}

fun add(r: Rsdv, x: Double): Rsdv {
    var n1: Double = r.n + 1.0
    var a1: Double = r.a + ((x - r.a) / n1)
    var q1: Double = r.q + ((x - r.a) * (x - a1))
    return Rsdv(n = n1, a = a1, q = q1)
}

fun sd(r: Rsdv): Double {
    return sqrtApprox(r.q / r.n)
}

fun user_main(): Unit {
    var r: Rsdv = newRsdv()
    for (x in mutableListOf(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)) {
        r = add(r, x)
        println(sd(r).toString())
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
