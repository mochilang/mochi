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

fun floyd(n: Int): String {
    var result: String = ""
    var i: Int = (0).toInt()
    while (i < n) {
        var j: Int = (0).toInt()
        while (j < ((n - i) - 1)) {
            result = result + " "
            j = j + 1
        }
        var k: Int = (0).toInt()
        while (k < (i + 1)) {
            result = result + "* "
            k = k + 1
        }
        result = result + "\n"
        i = i + 1
    }
    return result
}

fun reverse_floyd(n: Int): String {
    var result: String = ""
    var i: Int = (n).toInt()
    while (i > 0) {
        var j: Int = (i).toInt()
        while (j > 0) {
            result = result + "* "
            j = j - 1
        }
        result = result + "\n"
        var k: BigInteger = (((n - i) + 1).toBigInteger())
        while (k.compareTo((0).toBigInteger()) > 0) {
            result = result + " "
            k = k.subtract((1).toBigInteger())
        }
        i = i - 1
    }
    return result
}

fun pretty_print(n: Int): String {
    if (n <= 0) {
        return "       ...       ....        nothing printing :("
    }
    var upper_half: String = floyd(n)
    var lower_half: String = reverse_floyd(n)
    return upper_half + lower_half
}

fun user_main(): Unit {
    println(pretty_print(3))
    println(pretty_print(0))
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
