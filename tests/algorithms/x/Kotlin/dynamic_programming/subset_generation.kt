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

fun copy_list(src: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < src.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(src[i]!!); _tmp }
        i = i + 1
    }
    return result
}

fun subset_combinations(elements: MutableList<Int>, n: Int): MutableList<MutableList<Int>> {
    var r: Int = (elements.size).toInt()
    if (n > r) {
        return mutableListOf<MutableList<Int>>()
    }
    var dp: MutableList<MutableList<MutableList<Int>>> = mutableListOf<MutableList<MutableList<Int>>>()
    var i: Int = (0).toInt()
    while (i <= r) {
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(mutableListOf<MutableList<Int>>()); _tmp }
        i = i + 1
    }
    _listSet(dp, 0, run { val _tmp = (dp[0]!!).toMutableList(); _tmp.add(mutableListOf<Int>()); _tmp })
    i = 1
    while (i <= r) {
        var j: Int = (i).toInt()
        while (j > 0) {
            var prevs: MutableList<MutableList<Int>> = dp[j - 1]!!
            var k: Int = (0).toInt()
            while (k < prevs.size) {
                var prev: MutableList<Int> = prevs[k]!!
                var comb: MutableList<Int> = copy_list(prev)
                comb = run { val _tmp = comb.toMutableList(); _tmp.add(elements[i - 1]!!); _tmp }
                _listSet(dp, j, run { val _tmp = (dp[j]!!).toMutableList(); _tmp.add(comb); _tmp })
                k = k + 1
            }
            j = j - 1
        }
        i = i + 1
    }
    return dp[n]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(subset_combinations(mutableListOf(10, 20, 30, 40), 2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
