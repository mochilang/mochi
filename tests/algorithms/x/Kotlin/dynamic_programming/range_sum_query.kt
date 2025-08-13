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

data class Query(var left: Int = 0, var right: Int = 0)
var arr1: MutableList<Int> = mutableListOf(1, 4, 6, 2, 61, 12)
var queries1: MutableList<Query> = mutableListOf(Query(left = 2, right = 5), Query(left = 1, right = 5), Query(left = 3, right = 4))
fun prefix_sum(arr: MutableList<Int>, queries: MutableList<Query>): MutableList<Int> {
    var dp: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < arr.size) {
        if (i == 0) {
            dp = run { val _tmp = dp.toMutableList(); _tmp.add(arr[0]!!); _tmp }
        } else {
            dp = run { val _tmp = dp.toMutableList(); _tmp.add(dp[i - 1]!! + arr[i]!!); _tmp }
        }
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    var j: Int = (0).toInt()
    while (j < queries.size) {
        var q: Query = queries[j]!!
        var sum: Int = (dp[q.right]!!).toInt()
        if (q.left > 0) {
            sum = sum - dp[q.left - 1]!!
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(sum); _tmp }
        j = j + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(prefix_sum(arr1, queries1).toString())
        var arr2: MutableList<Int> = mutableListOf(4, 2, 1, 6, 3)
        var queries2: MutableList<Query> = mutableListOf(Query(left = 3, right = 4), Query(left = 1, right = 3), Query(left = 0, right = 2))
        println(prefix_sum(arr2, queries2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
