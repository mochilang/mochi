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

var amount: Int = 10
fun countChange(amount: Int): Int {
    var ways: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i <= amount) {
        ways = run { val _tmp = ways.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    ways[0] = 1
    var coins: MutableList<Int> = mutableListOf(1, 5, 10, 25)
    var idx: Int = 0
    while (idx < coins.size) {
        var coin: Int = coins[idx]!!
        var j: Int = coin
        while (j <= amount) {
            ways[j] = ways[j]!! + ways[j - coin]!!
            j = j + 1
        }
        idx = idx + 1
    }
    return ways[amount]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((("amount, ways to make change: " + amount.toString()) + " ") + countChange(amount).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
