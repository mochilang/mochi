val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/electronics"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun capacitor_parallel(capacitors: MutableList<Double>): Double {
    var sum_c: Double = 0.0
    var i: Int = (0).toInt()
    while (i < capacitors.size) {
        var c: Double = capacitors[i]!!
        if (c < 0.0) {
            panic(("Capacitor at index " + _numToStr(i)) + " has a negative value!")
            return 0.0
        }
        sum_c = sum_c + c
        i = i + 1
    }
    return sum_c
}

fun capacitor_series(capacitors: MutableList<Double>): Double {
    var first_sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < capacitors.size) {
        var c: Double = capacitors[i]!!
        if (c <= 0.0) {
            panic(("Capacitor at index " + _numToStr(i)) + " has a negative or zero value!")
            return 0.0
        }
        first_sum = first_sum + (1.0 / c)
        i = i + 1
    }
    return 1.0 / first_sum
}

fun user_main(): Unit {
    var parallel: Double = capacitor_parallel(mutableListOf(5.71389, 12.0, 3.0))
    var series: Double = capacitor_series(mutableListOf(5.71389, 12.0, 3.0))
    println(_numToStr(parallel))
    println(_numToStr(series))
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
