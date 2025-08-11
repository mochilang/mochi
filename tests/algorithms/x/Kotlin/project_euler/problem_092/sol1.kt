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

fun next_number(number: Int): Int {
    var n: Int = (number).toInt()
    var total: Int = (0).toInt()
    while (n > 0) {
        var d: Int = (Math.floorMod(n, 10)).toInt()
        total = total + (d * d)
        n = n / 10
    }
    return total
}

fun chain(number: Int): Boolean {
    var n: Int = (number).toInt()
    while ((n != 1) && (n != 89)) {
        n = next_number(n)
    }
    return n == 1
}

fun solution(limit: Int): Int {
    var count: Int = (0).toInt()
    var i: Int = (1).toInt()
    while (i < limit) {
        if (!chain(i)) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(next_number(44).toString())
        println(next_number(10).toString())
        println(next_number(32).toString())
        println(chain(10).toString())
        println(chain(58).toString())
        println(chain(1).toString())
        println(solution(100).toString())
        println(solution(1000).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
