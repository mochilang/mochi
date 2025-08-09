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

fun make_list(len: Int, value: Int): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < len) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return arr
}

fun count_recursive(array: MutableList<Int>, target: Int): Int {
    if (target < 0) {
        return 0
    }
    if (target == 0) {
        return 1
    }
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < array.size) {
        total = total + count_recursive(array, target - array[i]!!)
        i = i + 1
    }
    return total
}

fun combination_sum_iv(array: MutableList<Int>, target: Int): Int {
    return count_recursive(array, target)
}

fun count_dp(array: MutableList<Int>, target: Int, dp: MutableList<Int>): Int {
    if (target < 0) {
        return 0
    }
    if (target == 0) {
        return 1
    }
    if (dp[target]!! > (0 - 1)) {
        return dp[target]!!
    }
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < array.size) {
        total = total + count_dp(array, target - array[i]!!, dp)
        i = i + 1
    }
    _listSet(dp, target, total)
    return total
}

fun combination_sum_iv_dp_array(array: MutableList<Int>, target: Int): Int {
    var dp: MutableList<Int> = make_list(target + 1, 0 - 1)
    return count_dp(array, target, dp)
}

fun combination_sum_iv_bottom_up(n: Int, array: MutableList<Int>, target: Int): Int {
    var dp: MutableList<Int> = make_list(target + 1, 0)
    _listSet(dp, 0, 1)
    var i: Int = (1).toInt()
    while (i <= target) {
        var j: Int = (0).toInt()
        while (j < n) {
            if ((i - array[j]!!) >= 0) {
                _listSet(dp, i, dp[i]!! + dp[i - array[j]!!]!!)
            }
            j = j + 1
        }
        i = i + 1
    }
    return dp[target]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(combination_sum_iv(mutableListOf(1, 2, 5), 5).toString())
        println(combination_sum_iv_dp_array(mutableListOf(1, 2, 5), 5).toString())
        println(combination_sum_iv_bottom_up(3, mutableListOf(1, 2, 5), 5).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
