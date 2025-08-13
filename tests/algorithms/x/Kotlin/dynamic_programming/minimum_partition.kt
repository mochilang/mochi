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

fun find_min(numbers: MutableList<Int>): Int {
    var n: Int = (numbers.size).toInt()
    var s: Int = (0).toInt()
    var idx: Int = (0).toInt()
    while (idx < n) {
        s = s + numbers[idx]!!
        idx = idx + 1
    }
    var dp: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i <= n) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j <= s) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 0
    while (i <= n) {
        _listSet(dp[i]!!, 0, true)
        i = i + 1
    }
    var j: Int = (1).toInt()
    while (j <= s) {
        _listSet(dp[0]!!, j, false)
        j = j + 1
    }
    i = 1
    while (i <= n) {
        j = 1
        while (j <= s) {
            _listSet(dp[i]!!, j, (((dp[i - 1]!!) as MutableList<Boolean>))[j]!!)
            if (numbers[i - 1]!! <= j) {
                if ((((((dp[i - 1]!!) as MutableList<Boolean>))[j - numbers[i - 1]!!]!!) as Boolean)) {
                    _listSet(dp[i]!!, j, true)
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    var diff: Int = (0).toInt()
    j = s / 2
    while (j >= 0) {
        if ((((((dp[n]!!) as MutableList<Boolean>))[j]!!) as Boolean)) {
            diff = s - (2 * j)
            break
        }
        j = j - 1
    }
    return diff
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_min(mutableListOf(1, 2, 3, 4, 5)).toString())
        println(find_min(mutableListOf(5, 5, 5, 5, 5)).toString())
        println(find_min(mutableListOf(5, 5, 5, 5)).toString())
        println(find_min(mutableListOf(3)).toString())
        println(find_min(mutableListOf<Int>()).toString())
        println(find_min(mutableListOf(1, 2, 3, 4)).toString())
        println(find_min(mutableListOf(0, 0, 0, 0)).toString())
        println(find_min(mutableListOf(0 - 1, 0 - 5, 5, 1)).toString())
        println(find_min(mutableListOf(9, 9, 9, 9, 9)).toString())
        println(find_min(mutableListOf(1, 5, 10, 3)).toString())
        println(find_min(mutableListOf(0 - 1, 0, 1)).toString())
        println(find_min(mutableListOf(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
