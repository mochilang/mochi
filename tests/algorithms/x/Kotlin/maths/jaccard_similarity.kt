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

fun contains(xs: MutableList<String>, value: String): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == value) {
            return true
        }
        i = i + 1
    }
    return false
}

fun jaccard_similarity(set_a: MutableList<String>, set_b: MutableList<String>, alternative_union: Boolean): Double {
    var intersection_len: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < set_a.size) {
        if ((contains(set_b, set_a[i]!!)) as Boolean) {
            intersection_len = intersection_len + 1
        }
        i = i + 1
    }
    var union_len: Int = (0).toInt()
    if (alternative_union as Boolean) {
        union_len = set_a.size + set_b.size
    } else {
        var union_list: MutableList<String> = mutableListOf<String>()
        i = 0
        while (i < set_a.size) {
            var val_a: String = set_a[i]!!
            if (!contains(union_list, val_a)) {
                union_list = run { val _tmp = union_list.toMutableList(); _tmp.add(val_a); _tmp }
            }
            i = i + 1
        }
        i = 0
        while (i < set_b.size) {
            var val_b: String = set_b[i]!!
            if (!contains(union_list, val_b)) {
                union_list = run { val _tmp = union_list.toMutableList(); _tmp.add(val_b); _tmp }
            }
            i = i + 1
        }
        union_len = union_list.size
    }
    return (1.0 * (intersection_len).toDouble()) / (union_len).toDouble()
}

fun user_main(): Unit {
    var set_a: MutableList<String> = mutableListOf("a", "b", "c", "d", "e")
    var set_b: MutableList<String> = mutableListOf("c", "d", "e", "f", "h", "i")
    println(jaccard_similarity(set_a, set_b, false))
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
