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

fun sum_of_digits(n: Int): Int {
    var m: Int = (abs_int(n)).toInt()
    var res: Int = (0).toInt()
    while (m > 0) {
        res = res + (Math.floorMod(m, 10))
        m = m / 10
    }
    return res
}

fun sum_of_digits_recursion(n: Int): Int {
    var m: Int = (abs_int(n)).toInt()
    if (m < 10) {
        return m
    }
    return (Math.floorMod(m, 10)) + sum_of_digits_recursion(m / 10)
}

fun sum_of_digits_compact(n: Int): Int {
    var s: String = _numToStr(abs_int(n))
    var res: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        res = res + s[i].toString().toInt()
        i = i + 1
    }
    return res
}

fun test_sum_of_digits(): Unit {
    if (sum_of_digits(12345) != 15) {
        panic("sum_of_digits 12345 failed")
    }
    if (sum_of_digits(123) != 6) {
        panic("sum_of_digits 123 failed")
    }
    if (sum_of_digits(0 - 123) != 6) {
        panic("sum_of_digits -123 failed")
    }
    if (sum_of_digits(0) != 0) {
        panic("sum_of_digits 0 failed")
    }
    if (sum_of_digits_recursion(12345) != 15) {
        panic("recursion 12345 failed")
    }
    if (sum_of_digits_recursion(123) != 6) {
        panic("recursion 123 failed")
    }
    if (sum_of_digits_recursion(0 - 123) != 6) {
        panic("recursion -123 failed")
    }
    if (sum_of_digits_recursion(0) != 0) {
        panic("recursion 0 failed")
    }
    if (sum_of_digits_compact(12345) != 15) {
        panic("compact 12345 failed")
    }
    if (sum_of_digits_compact(123) != 6) {
        panic("compact 123 failed")
    }
    if (sum_of_digits_compact(0 - 123) != 6) {
        panic("compact -123 failed")
    }
    if (sum_of_digits_compact(0) != 0) {
        panic("compact 0 failed")
    }
}

fun user_main(): Unit {
    test_sum_of_digits()
    println(_numToStr(sum_of_digits(12345)))
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
