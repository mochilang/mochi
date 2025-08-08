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

var arr1: MutableList<Int> = mutableListOf(0 - 7, 1, 5, 2, 0 - 4, 3, 0)
fun equilibrium_index(arr: MutableList<Int>): Int {
    var total: Int = 0
    var i: Int = 0
    while (i < arr.size) {
        total = total + arr[i]!!
        i = i + 1
    }
    var left: Int = 0
    i = 0
    while (i < arr.size) {
        total = total - arr[i]!!
        if (left == total) {
            return i
        }
        left = left + arr[i]!!
        i = i + 1
    }
    return 0 - 1
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(equilibrium_index(arr1))
        var arr2: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5)
        println(equilibrium_index(arr2))
        var arr3: MutableList<Int> = mutableListOf(1, 1, 1, 1, 1)
        println(equilibrium_index(arr3))
        var arr4: MutableList<Int> = mutableListOf(2, 4, 6, 8, 10, 3)
        println(equilibrium_index(arr4))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
