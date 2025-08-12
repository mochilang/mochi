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

fun is_palindrome(num: Int): Boolean {
    if (num < 0) {
        return false
    }
    var n: Int = (num).toInt()
    var rev: Int = (0).toInt()
    while (n > 0) {
        rev = (rev * 10) + (Math.floorMod(n, 10))
        n = n / 10
    }
    return rev == num
}

fun solution(limit: Int): Int {
    var answer: Int = (0).toInt()
    var i: Int = (999).toInt()
    while (i >= 100) {
        var j: Int = (999).toInt()
        while (j >= 100) {
            var product: Int = (i * j).toInt()
            if ((((product < limit) && is_palindrome(product) as Boolean)) && (product > answer)) {
                answer = product
            }
            j = j - 1
        }
        i = i - 1
    }
    return answer
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(solution(998001).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
