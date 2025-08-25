val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

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

fun copy_list(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun longest_subsequence(arr: MutableList<Int>): MutableList<Int> {
    var n: Int = (arr.size).toInt()
    var lis: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var single: MutableList<Int> = mutableListOf<Int>()
        single = run { val _tmp = single.toMutableList(); _tmp.add(arr[i]!!); _tmp }
        lis = run { val _tmp = lis.toMutableList(); _tmp.add(single); _tmp }
        i = i + 1
    }
    i = 1
    while (i < n) {
        var prev: Int = (0).toInt()
        while (prev < i) {
            if ((arr[prev]!! <= arr[i]!!) && (((lis[prev]!!).size + 1) > (lis[i]!!).size)) {
                var temp: MutableList<Int> = copy_list(lis[prev]!!)
                var temp2 = run { val _tmp = temp.toMutableList(); _tmp.add(arr[i]!!); _tmp }
                _listSet(lis, i, temp2 as MutableList<Int>)
            }
            prev = prev + 1
        }
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < n) {
        if ((lis[i]!!).size > result.size) {
            result = lis[i]!!
        }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    println(longest_subsequence(mutableListOf(10, 22, 9, 33, 21, 50, 41, 60, 80)).toString())
    println(longest_subsequence(mutableListOf(4, 8, 7, 5, 1, 12, 2, 3, 9)).toString())
    println(longest_subsequence(mutableListOf(9, 8, 7, 6, 5, 7)).toString())
    println(longest_subsequence(mutableListOf(28, 26, 12, 23, 35, 39)).toString())
    println(longest_subsequence(mutableListOf(1, 1, 1)).toString())
    println(longest_subsequence(mutableListOf<Int>()).toString())
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
