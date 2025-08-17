import java.math.BigInteger

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

fun integer_square_root(num: Int): Int {
    if (num < 0) {
        panic("num must be non-negative integer")
    }
    if (num < 2) {
        return num
    }
    var left_bound: Int = (0).toInt()
    var right_bound: Int = (num / 2).toInt()
    while (left_bound <= right_bound) {
        var mid: Int = (left_bound + ((right_bound - left_bound) / 2)).toInt()
        var mid_squared: Long = (mid).toLong() * (mid).toLong()
        if (mid_squared == (num).toLong()) {
            return mid
        }
        if (mid_squared < num) {
            left_bound = mid + 1
        } else {
            right_bound = mid - 1
        }
    }
    return right_bound
}

fun test_integer_square_root(): Unit {
    var expected: MutableList<Int> = mutableListOf(0, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4)
    var i: Int = (0).toInt()
    while (i < expected.size) {
        var result: Int = (integer_square_root(i)).toInt()
        if (result != expected[i]!!) {
            panic("test failed at index " + _numToStr(i))
        }
        i = i + 1
    }
    if (integer_square_root(625) != 25) {
        panic("sqrt of 625 incorrect")
    }
    if (integer_square_root(2147483647) != 46340) {
        panic("sqrt of max int incorrect")
    }
}

fun user_main(): Unit {
    test_integer_square_root()
    println(_numToStr(integer_square_root(625)))
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
