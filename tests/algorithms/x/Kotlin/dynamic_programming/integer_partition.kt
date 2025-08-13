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

fun partition(m: Int): Int {
    var memo: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < (m + 1)) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < m) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        memo = run { val _tmp = memo.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 0
    while (i < (m + 1)) {
        _listSet(memo[i]!!, 0, 1)
        i = i + 1
    }
    var n: Int = (0).toInt()
    while (n < (m + 1)) {
        var k: Int = (1).toInt()
        while (k < m) {
            _listSet(memo[n]!!, k, (((memo[n]!!) as MutableList<Int>))[k]!! + (((memo[n]!!) as MutableList<Int>))[k - 1]!!)
            if ((n - k) > 0) {
                _listSet(memo[n]!!, k, (((memo[n]!!) as MutableList<Int>))[k]!! + (((memo[(n - k) - 1]!!) as MutableList<Int>))[k]!!)
            }
            k = k + 1
        }
        n = n + 1
    }
    return (((memo[m]!!) as MutableList<Int>))[m - 1]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(partition(5))
        println(partition(7))
        println(partition(100))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
