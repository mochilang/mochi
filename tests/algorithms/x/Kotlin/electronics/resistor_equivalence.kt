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

fun resistor_parallel(resistors: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < resistors.size) {
        var r: Double = resistors[i]!!
        if (r <= 0.0) {
            panic(("Resistor at index " + i.toString()) + " has a negative or zero value!")
        }
        sum = sum + (1.0 / r)
        i = i + 1
    }
    return 1.0 / sum
}

fun resistor_series(resistors: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < resistors.size) {
        var r: Double = resistors[i]!!
        if (r < 0.0) {
            panic(("Resistor at index " + i.toString()) + " has a negative value!")
        }
        sum = sum + r
        i = i + 1
    }
    return sum
}

fun user_main(): Unit {
    var resistors: MutableList<Double> = mutableListOf(3.21389, 2.0, 3.0)
    println("Parallel: " + resistor_parallel(resistors).toString())
    println("Series: " + resistor_series(resistors).toString())
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
