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

fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun maxf(a: Double, b: Double): Double {
    if (a > b) {
        return a
    }
    return b
}

fun isClose(a: Double, b: Double): Boolean {
    val relTol: Double = 0.000000001
    val t: Double = kotlin.math.abs(a - b)
    val u: Double = relTol * maxf(kotlin.math.abs(a), kotlin.math.abs(b))
    return t <= u
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun user_main(): Unit {
    val root2: Double = sqrtApprox(2.0)
    val pairs: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(100000000000000.02, 100000000000000.02), mutableListOf(100.01, 100.011), mutableListOf(10000000000000.002 / 10000.0, 1000000000.0000001), mutableListOf(0.001, 0.0010000001), mutableListOf(0.000000000000000000000101, 0.0), mutableListOf(root2 * root2, 2.0), mutableListOf((0.0 - root2) * root2, 0.0 - 2.0), mutableListOf(100000000000000000.0, 100000000000000000.0), mutableListOf(3.141592653589793, 3.141592653589793))
    for (pair in pairs) {
        val a: Double = pair[0]
        val b: Double = pair[1]
        val s: String = if (isClose(a, b) != null) "≈" else "≉"
        println((((a.toString() + " ") + s) + " ") + b.toString())
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
