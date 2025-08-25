import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun max_int(a: Int, b: Int): Int {
    if (a > b) {
        return a
    } else {
        return b
    }
}

fun min_int(a: Int, b: Int): Int {
    if (a < b) {
        return a
    } else {
        return b
    }
}

fun min3(a: Int, b: Int, c: Int): Int {
    return min_int(min_int(a, b), c)
}

fun minimum_tickets_cost(days: MutableList<Int>, costs: MutableList<Int>): Int {
    if (days.size == 0) {
        return 0
    }
    var last_day: Int = (days[days.size - 1]!!).toInt()
    var dp: MutableList<Int> = make_list(last_day + 1, 0)
    var day_index: Int = (0).toInt()
    var d: Int = (1).toInt()
    while (d <= last_day) {
        if ((day_index < days.size) && (d == days[day_index]!!)) {
            var cost1: Int = (dp[d - 1]!! + costs[0]!!).toInt()
            var cost7: Int = (dp[max_int(0, d - 7)]!! + costs[1]!!).toInt()
            var cost30: Int = (dp[max_int(0, d - 30)]!! + costs[2]!!).toInt()
            _listSet(dp, d, min3(cost1, cost7, cost30))
            day_index = day_index + 1
        } else {
            _listSet(dp, d, dp[d - 1]!!)
        }
        d = d + 1
    }
    return dp[last_day]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(minimum_tickets_cost(mutableListOf(1, 4, 6, 7, 8, 20), mutableListOf(2, 7, 15))))
        println(_numToStr(minimum_tickets_cost(mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31), mutableListOf(2, 7, 15))))
        println(_numToStr(minimum_tickets_cost(mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31), mutableListOf(2, 90, 150))))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
