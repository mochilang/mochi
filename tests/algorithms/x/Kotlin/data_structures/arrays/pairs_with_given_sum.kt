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

fun pairs_with_sum(arr: MutableList<Int>, req_sum: Int): Int {
    var n: Int = (arr.size).toInt()
    var count: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((n).toBigInteger()) < 0) {
            if ((arr[i]!! + arr[(j).toInt()]!!) == req_sum) {
                count = count + 1
            }
            j = j.add((1).toBigInteger())
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
        println(pairs_with_sum(mutableListOf(1, 5, 7, 1), 6))
        println(pairs_with_sum(mutableListOf(1, 1, 1, 1, 1, 1, 1, 1), 2))
        println(pairs_with_sum(mutableListOf(1, 7, 6, 2, 5, 4, 3, 1, 9, 8), 7))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
