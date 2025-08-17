fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun max_sum_sliding_window(arr: MutableList<Int>, k: Int): Int {
    if ((k < 0) || (arr.size < k)) {
        panic("Invalid Input")
    }
    var idx: Int = (0).toInt()
    var current_sum: Int = (0).toInt()
    while (idx < k) {
        current_sum = current_sum + arr[idx]!!
        idx = idx + 1
    }
    var max_sum: Int = (current_sum).toInt()
    var i: Int = (0).toInt()
    while (i < (arr.size - k)) {
        current_sum = (current_sum - arr[i]!!) + arr[i + k]!!
        if (current_sum > max_sum) {
            max_sum = current_sum
        }
        i = i + 1
    }
    return max_sum
}

fun test_max_sum_sliding_window(): Unit {
    var arr1: MutableList<Int> = mutableListOf(1, 4, 2, 10, 2, 3, 1, 0, 20)
    if (max_sum_sliding_window(arr1, 4) != 24) {
        panic("test1 failed")
    }
    var arr2: MutableList<Int> = mutableListOf(1, 4, 2, 10, 2, 13, 1, 0, 2)
    if (max_sum_sliding_window(arr2, 4) != 27) {
        panic("test2 failed")
    }
}

fun user_main(): Unit {
    test_max_sum_sliding_window()
    var sample: MutableList<Int> = mutableListOf(1, 4, 2, 10, 2, 3, 1, 0, 20)
    println(_numToStr(max_sum_sliding_window(sample, 4)))
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
