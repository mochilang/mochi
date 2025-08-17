import java.math.BigInteger

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

fun perfect_square(num: Int): Boolean {
    if (num < 0) {
        return false
    }
    var i: Int = (0).toInt()
    while (((i).toLong() * (i).toLong()) <= num) {
        if (((i).toLong() * (i).toLong()) == (num).toLong()) {
            return true
        }
        i = i + 1
    }
    return false
}

fun perfect_square_binary_search(n: Int): Boolean {
    if (n < 0) {
        return false
    }
    var left: Int = (0).toInt()
    var right: Int = (n).toInt()
    while (left <= right) {
        var mid: Int = ((left + right) / 2).toInt()
        var sq: Long = (mid).toLong() * (mid).toLong()
        if (sq == (n).toLong()) {
            return true
        }
        if (sq > n) {
            right = mid - 1
        } else {
            left = mid + 1
        }
    }
    return false
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(perfect_square(9).toString())
        println(perfect_square(10).toString())
        println(perfect_square_binary_search(16).toString())
        println(perfect_square_binary_search(10).toString())
        println(perfect_square_binary_search(0 - 1).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
