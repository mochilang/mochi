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

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun decimal_isolate(number: Double, digit_amount: Int): Double {
    var whole: Int = ((number.toInt())).toInt()
    var frac: Double = number - ((whole.toDouble()))
    if (digit_amount > 0) {
        return round(frac, digit_amount)
    }
    return frac
}

fun user_main(): Unit {
    println(_numToStr(decimal_isolate(1.53, 0)))
    println(_numToStr(decimal_isolate(35.345, 1)))
    println(_numToStr(decimal_isolate(35.345, 2)))
    println(_numToStr(decimal_isolate(35.345, 3)))
    println(_numToStr(decimal_isolate(0.0 - 14.789, 3)))
    println(_numToStr(decimal_isolate(0.0, 2)))
    println(_numToStr(decimal_isolate(0.0 - 14.123, 1)))
    println(_numToStr(decimal_isolate(0.0 - 14.123, 2)))
    println(_numToStr(decimal_isolate(0.0 - 14.123, 3)))
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
