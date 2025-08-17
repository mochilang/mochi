import java.math.BigInteger

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

fun contains(xs: MutableList<Int>, value: Int): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == value) {
            return true
        }
        i = i + 1
    }
    return false
}

fun sumset(set_a: MutableList<Int>, set_b: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < set_a.size) {
        var j: Int = (0).toInt()
        while (j < set_b.size) {
            var s: Int = (set_a[i]!! + set_b[j]!!).toInt()
            if (!contains(result, s)) {
                result = run { val _tmp = result.toMutableList(); _tmp.add(s); _tmp }
            }
            j = j + 1
        }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var set_a: MutableList<Int> = mutableListOf(1, 2, 3)
    var set_b: MutableList<Int> = mutableListOf(4, 5, 6)
    println(sumset(set_a, set_b).toString())
    var set_c: MutableList<Int> = mutableListOf(4, 5, 6, 7)
    println(sumset(set_a, set_c).toString())
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
