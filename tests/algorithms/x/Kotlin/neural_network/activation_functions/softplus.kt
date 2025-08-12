fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun ln(x: Double): Double {
    if (x <= 0.0) {
        panic("ln domain error")
    }
    var y: Double = (x - 1.0) / (x + 1.0)
    var y2: Double = y * y
    var term: Double = y
    var sum: Double = 0.0
    var k: Int = (0).toInt()
    while (k < 10) {
        var denom: Double = (((2 * k) + 1).toDouble())
        sum = sum + (term / denom)
        term = term * y2
        k = k + 1
    }
    return 2.0 * sum
}

fun exp(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun softplus(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var value: Double = ln(1.0 + exp(x))
        result = run { val _tmp = result.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var v1: MutableList<Double> = mutableListOf(2.3, 0.6, 0.0 - 2.0, 0.0 - 3.8)
    var v2: MutableList<Double> = mutableListOf(0.0 - 9.2, 0.0 - 0.3, 0.45, 0.0 - 4.56)
    var r1: MutableList<Double> = softplus(v1)
    var r2: MutableList<Double> = softplus(v2)
    println(r1)
    println(r2)
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
