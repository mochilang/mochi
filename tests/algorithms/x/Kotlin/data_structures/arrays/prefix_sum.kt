fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

data class PrefixSum(var prefix_sum: MutableList<Int> = mutableListOf<Int>())
var ps: PrefixSum = make_prefix_sum(mutableListOf(1, 2, 3))
fun make_prefix_sum(arr: MutableList<Int>): PrefixSum {
    var prefix: MutableList<Int> = mutableListOf<Int>()
    var running: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < arr.size) {
        running = running + arr[i]!!
        prefix = run { val _tmp = prefix.toMutableList(); _tmp.add(running); _tmp }
        i = i + 1
    }
    return PrefixSum(prefix_sum = prefix)
}

fun get_sum(ps: PrefixSum, start: Int, end: Int): Int {
    var prefix: MutableList<Int> = ps.prefix_sum
    if (prefix.size == 0) {
        panic("The array is empty.")
    }
    if ((((start < 0) || (end >= prefix.size) as Boolean)) || (start > end)) {
        panic("Invalid range specified.")
    }
    if (start == 0) {
        return prefix[end]!!
    }
    return prefix[end]!! - prefix[start - 1]!!
}

fun contains_sum(ps: PrefixSum, target_sum: Int): Boolean {
    var prefix: MutableList<Int> = ps.prefix_sum
    var sums: MutableList<Int> = mutableListOf(0)
    var i: Int = (0).toInt()
    while (i < prefix.size) {
        var sum_item: Int = (prefix[i]!!).toInt()
        var j: Int = (0).toInt()
        while (j < sums.size) {
            if (sums[j]!! == (sum_item - target_sum)) {
                return true
            }
            j = j + 1
        }
        sums = run { val _tmp = sums.toMutableList(); _tmp.add(sum_item); _tmp }
        i = i + 1
    }
    return false
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(get_sum(ps, 0, 2).toString())
        println(get_sum(ps, 1, 2).toString())
        println(get_sum(ps, 2, 2).toString())
        println(contains_sum(ps, 6).toString())
        println(contains_sum(ps, 5).toString())
        println(contains_sum(ps, 3).toString())
        println(contains_sum(ps, 4).toString())
        println(contains_sum(ps, 7).toString())
        var ps2: PrefixSum = make_prefix_sum(mutableListOf(1, 0 - 2, 3))
        println(contains_sum(ps2, 2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
