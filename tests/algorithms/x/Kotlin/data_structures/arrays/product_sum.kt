fun _exists(v: Any?): Boolean {
    if (v == null) return false
    if (v is Collection<*>) return true
    return v.javaClass.isArray
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

var example: MutableList<Any?> = mutableListOf<Any?>((5 as Any?), (2 as Any?), (mutableListOf(0 - 7, 1) as Any?), (3 as Any?), (mutableListOf<Any?>((6 as Any?), (mutableListOf(0 - 13, 8) as Any?), (4 as Any?)) as Any?))
fun product_sum(arr: MutableList<Any?>, depth: Int): Int {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var el: Any? = arr[i] as Any?
        if (_exists(el)) {
            total = total + product_sum((el as MutableList<Any?>), depth + 1)
        } else {
            total = total + ((el as Int))
        }
        i = i + 1
    }
    return total * depth
}

fun product_sum_array(array: MutableList<Any?>): Int {
    var res: Int = (product_sum(array, 1)).toInt()
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(product_sum_array(example))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
