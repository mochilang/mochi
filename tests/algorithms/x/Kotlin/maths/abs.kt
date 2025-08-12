fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun abs_val(num: Double): Double {
    if (num < 0.0) {
        return 0.0 - num
    }
    return num
}

fun abs_min(x: MutableList<Int>): Int {
    if (x.size == 0) {
        panic("abs_min() arg is an empty sequence")
    }
    var j: Int = (x[0]!!).toInt()
    var idx: Int = (0).toInt()
    while (idx < x.size) {
        var i: Int = (x[idx]!!).toInt()
        if (abs_val((i.toDouble())) < abs_val((j.toDouble()))) {
            j = i
        }
        idx = idx + 1
    }
    return j
}

fun abs_max(x: MutableList<Int>): Int {
    if (x.size == 0) {
        panic("abs_max() arg is an empty sequence")
    }
    var j: Int = (x[0]!!).toInt()
    var idx: Int = (0).toInt()
    while (idx < x.size) {
        var i: Int = (x[idx]!!).toInt()
        if (abs_val((i.toDouble())) > abs_val((j.toDouble()))) {
            j = i
        }
        idx = idx + 1
    }
    return j
}

fun abs_max_sort(x: MutableList<Int>): Int {
    if (x.size == 0) {
        panic("abs_max_sort() arg is an empty sequence")
    }
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < x.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(x[i]!!); _tmp }
        i = i + 1
    }
    var n: Int = (arr.size).toInt()
    var a: Int = (0).toInt()
    while (a < n) {
        var b: Int = (0).toInt()
        while (b < ((n - a) - 1)) {
            if (abs_val(((arr[b]!!).toDouble())) > abs_val(((arr[b + 1]!!).toDouble()))) {
                var temp: Int = (arr[b]!!).toInt()
                _listSet(arr, b, arr[b + 1]!!)
                _listSet(arr, b + 1, temp)
            }
            b = b + 1
        }
        a = a + 1
    }
    return arr[n - 1]!!
}

fun test_abs_val(): Unit {
    if (abs_val(0.0) != 0.0) {
        panic("abs_val(0) failed")
    }
    if (abs_val(34.0) != 34.0) {
        panic("abs_val(34) failed")
    }
    if (abs_val(0.0 - 100000000000.0) != 100000000000.0) {
        panic("abs_val large failed")
    }
    var a: MutableList<Int> = mutableListOf(0 - 3, 0 - 1, 2, 0 - 11)
    if (abs_max(a) != (0 - 11)) {
        panic("abs_max failed")
    }
    if (abs_max_sort(a) != (0 - 11)) {
        panic("abs_max_sort failed")
    }
    if (abs_min(a) != (0 - 1)) {
        panic("abs_min failed")
    }
}

fun user_main(): Unit {
    test_abs_val()
    println(abs_val(0.0 - 34.0))
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
