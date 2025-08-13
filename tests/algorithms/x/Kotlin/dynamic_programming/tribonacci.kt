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

fun tribonacci(num: Int): MutableList<Int> {
    var dp: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < num) {
        if ((i == 0) || (i == 1)) {
            dp = run { val _tmp = dp.toMutableList(); _tmp.add(0); _tmp }
        } else {
            if (i == 2) {
                dp = run { val _tmp = dp.toMutableList(); _tmp.add(1); _tmp }
            } else {
                var t: Int = ((dp[i - 1]!! + dp[i - 2]!!) + dp[i - 3]!!).toInt()
                dp = run { val _tmp = dp.toMutableList(); _tmp.add(t); _tmp }
            }
        }
        i = i + 1
    }
    return dp
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(tribonacci(8))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
