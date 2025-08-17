fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun abs_val(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun validate_point(p: MutableList<Double>): Unit {
    if (p.size == 0) {
        panic("Missing an input")
    }
}

fun manhattan_distance(a: MutableList<Double>, b: MutableList<Double>): Double {
    validate_point(a)
    validate_point(b)
    if (a.size != b.size) {
        panic("Both points must be in the same n-dimensional space")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        total = total + abs_val(a[i]!! - b[i]!!)
        i = i + 1
    }
    return total
}

fun manhattan_distance_one_liner(a: MutableList<Double>, b: MutableList<Double>): Double {
    return manhattan_distance(a, b)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(manhattan_distance(mutableListOf(1.0, 1.0), mutableListOf(2.0, 2.0))))
        println(_numToStr(manhattan_distance(mutableListOf(1.5, 1.5), mutableListOf(2.0, 2.0))))
        println(_numToStr(manhattan_distance_one_liner(mutableListOf(1.5, 1.5), mutableListOf(2.5, 2.0))))
        println(_numToStr(manhattan_distance_one_liner(mutableListOf(0.0 - 3.0, 0.0 - 3.0, 0.0 - 3.0), mutableListOf(0.0, 0.0, 0.0))))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
