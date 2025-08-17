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

fun abs_int(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun num_digits(n: Int): Int {
    var x: Int = (abs_int(n)).toInt()
    var digits: Int = (1).toInt()
    while (x >= 10) {
        x = x / 10
        digits = digits + 1
    }
    return digits
}

fun num_digits_fast(n: Int): Int {
    var x: Int = (abs_int(n)).toInt()
    var digits: Int = (1).toInt()
    var power: Int = (10).toInt()
    while (x >= power) {
        power = power * 10
        digits = digits + 1
    }
    return digits
}

fun num_digits_faster(n: Int): Int {
    var s: String = _numToStr(abs_int(n))
    return s.length
}

fun test_num_digits(): Unit {
    if (num_digits(12345) != 5) {
        panic("num_digits 12345 failed")
    }
    if (num_digits(123) != 3) {
        panic("num_digits 123 failed")
    }
    if (num_digits(0) != 1) {
        panic("num_digits 0 failed")
    }
    if (num_digits(0 - 1) != 1) {
        panic("num_digits -1 failed")
    }
    if (num_digits(0 - 123456) != 6) {
        panic("num_digits -123456 failed")
    }
    if (num_digits_fast(12345) != 5) {
        panic("num_digits_fast 12345 failed")
    }
    if (num_digits_fast(123) != 3) {
        panic("num_digits_fast 123 failed")
    }
    if (num_digits_fast(0) != 1) {
        panic("num_digits_fast 0 failed")
    }
    if (num_digits_fast(0 - 1) != 1) {
        panic("num_digits_fast -1 failed")
    }
    if (num_digits_fast(0 - 123456) != 6) {
        panic("num_digits_fast -123456 failed")
    }
    if (num_digits_faster(12345) != 5) {
        panic("num_digits_faster 12345 failed")
    }
    if (num_digits_faster(123) != 3) {
        panic("num_digits_faster 123 failed")
    }
    if (num_digits_faster(0) != 1) {
        panic("num_digits_faster 0 failed")
    }
    if (num_digits_faster(0 - 1) != 1) {
        panic("num_digits_faster -1 failed")
    }
    if (num_digits_faster(0 - 123456) != 6) {
        panic("num_digits_faster -123456 failed")
    }
}

fun user_main(): Unit {
    test_num_digits()
    println(_numToStr(num_digits(12345)))
    println(_numToStr(num_digits_fast(12345)))
    println(_numToStr(num_digits_faster(12345)))
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
