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

fun powf(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun nthRoot(x: Double, n: Int): Double {
    var low: Double = 0.0
    var high: Double = x
    var i: Int = 0
    while (i < 60) {
        var mid: Double = (low + high) / 2.0
        if (powf(mid, n) > x) {
            high = mid
        } else {
            low = mid
        }
        i = i + 1
    }
    return low
}

fun user_main(): Unit {
    var sum: Double = 0.0
    var sumRecip: Double = 0.0
    var prod: Double = 1.0
    var n: Int = 1
    while (n <= 10) {
        var f: Double = n.toDouble()
        sum = sum + f
        sumRecip = sumRecip + (1.0 / f)
        prod = prod * f
        n = n + 1
    }
    var count: Double = 10.0
    var a: Double = sum / count
    var g: Double = nthRoot(prod, 10)
    var h: Double = count / sumRecip
    println((((("A: " + a.toString()) + " G: ") + g.toString()) + " H: ") + h.toString())
    println("A >= G >= H: " + ((a >= g) && (g >= h)).toString())
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
