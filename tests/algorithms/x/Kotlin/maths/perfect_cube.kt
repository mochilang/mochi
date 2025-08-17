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

fun perfect_cube(n: Int): Boolean {
    var m: Int = (n).toInt()
    if (m < 0) {
        m = 0 - m
    }
    var i: Int = (0).toInt()
    while ((((i).toLong() * (i).toLong()) * (i).toLong()) < m) {
        i = i + 1
    }
    return (((i).toLong() * (i).toLong()) * (i).toLong()) == (m).toLong()
}

fun perfect_cube_binary_search(n: Int): Boolean {
    var m: Int = (n).toInt()
    if (m < 0) {
        m = 0 - m
    }
    var left: Int = (0).toInt()
    var right: Int = (m).toInt()
    while (left <= right) {
        var mid: Int = (left + ((right - left) / 2)).toInt()
        var cube: Long = ((mid).toLong() * (mid).toLong()) * (mid).toLong()
        if (cube == (m).toLong()) {
            return true
        }
        if (cube < m) {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    return false
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(perfect_cube(27).toString())
        println(perfect_cube(4).toString())
        println(perfect_cube_binary_search(27).toString())
        println(perfect_cube_binary_search(64).toString())
        println(perfect_cube_binary_search(4).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
