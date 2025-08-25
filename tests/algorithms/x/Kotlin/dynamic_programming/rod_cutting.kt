import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var prices: MutableList<Int> = mutableListOf(1, 5, 8, 9, 10, 17, 17, 20, 24, 30)
fun enforce_args(n: Int, prices: MutableList<Int>): Unit {
    if (n < 0) {
        panic("n must be non-negative")
    }
    if (n > prices.size) {
        panic("price list is shorter than n")
    }
}

fun bottom_up_cut_rod(n: Int, prices: MutableList<Int>): Int {
    enforce_args(n, prices)
    var max_rev: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= n) {
        if (i == 0) {
            max_rev = run { val _tmp = max_rev.toMutableList(); _tmp.add(0); _tmp }
        } else {
            max_rev = run { val _tmp = max_rev.toMutableList(); _tmp.add((((0).toLong() - 2147483648L).toInt()).toInt()); _tmp }
        }
        i = i + 1
    }
    var length: Int = (1).toInt()
    while (length <= n) {
        var best: Int = (max_rev[length]!!).toInt()
        var j: Int = (1).toInt()
        while (j <= length) {
            var candidate: Int = (prices[j - 1]!! + max_rev[length - j]!!).toInt()
            if (candidate > best) {
                best = candidate
            }
            j = j + 1
        }
        _listSet(max_rev, length, best)
        length = length + 1
    }
    return max_rev[n]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(bottom_up_cut_rod(4, prices))
        println(bottom_up_cut_rod(10, prices))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
