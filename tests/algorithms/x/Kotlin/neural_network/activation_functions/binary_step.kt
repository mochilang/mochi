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

fun binary_step(vector: MutableList<Double>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        if (vector[i]!! >= 0.0) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(1); _tmp }
        } else {
            out = run { val _tmp = out.toMutableList(); _tmp.add(0); _tmp }
        }
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var vector: MutableList<Double> = mutableListOf(0.0 - 1.2, 0.0, 2.0, 1.45, 0.0 - 3.7, 0.3)
    var result: MutableList<Int> = binary_step(vector)
    println(result)
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
