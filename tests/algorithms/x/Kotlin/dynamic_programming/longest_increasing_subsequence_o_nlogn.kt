import java.math.BigInteger

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

fun ceil_index(v: MutableList<Int>, left: Int, right: Int, key: Int): Int {
    var l: Int = (left).toInt()
    var r: Int = (right).toInt()
    while ((r - l) > 1) {
        var middle: Int = ((l + r) / 2).toInt()
        if (v[middle]!! >= key) {
            r = middle
        } else {
            l = middle
        }
    }
    return r
}

fun longest_increasing_subsequence_length(v: MutableList<Int>): Int {
    if (v.size == 0) {
        return 0
    }
    var tail: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < v.size) {
        tail = run { val _tmp = tail.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var length: Int = (1).toInt()
    _listSet(tail, 0, v[0]!!)
    var j: Int = (1).toInt()
    while (j < v.size) {
        if (v[j]!! < tail[0]!!) {
            _listSet(tail, 0, v[j]!!)
        } else {
            if (v[j]!! > tail[length - 1]!!) {
                _listSet(tail, length, v[j]!!)
                length = length + 1
            } else {
                var idx: Int = (ceil_index(tail, 0 - 1, length - 1, v[j]!!)).toInt()
                _listSet(tail, idx, v[j]!!)
            }
        }
        j = j + 1
    }
    return length
}

fun user_main(): Unit {
    var example1: MutableList<Int> = mutableListOf(2, 5, 3, 7, 11, 8, 10, 13, 6)
    var example2: MutableList<Int> = mutableListOf<Int>()
    var example3: MutableList<Int> = mutableListOf(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
    var example4: MutableList<Int> = mutableListOf(5, 4, 3, 2, 1)
    println(longest_increasing_subsequence_length(example1))
    println(longest_increasing_subsequence_length(example2))
    println(longest_increasing_subsequence_length(example3))
    println(longest_increasing_subsequence_length(example4))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
