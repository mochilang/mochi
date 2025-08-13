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

fun repeat_char(c: String, count: Int): String {
    var s: String = ""
    var i: Int = (0).toInt()
    while (i < count) {
        s = s + c
        i = i + 1
    }
    return s
}

fun vicsek(order: Int): MutableList<String> {
    if (order == 0) {
        return mutableListOf("#")
    }
    var prev: MutableList<String> = vicsek(order - 1)
    var size: Int = (prev.size).toInt()
    var blank: String = repeat_char(" ", size)
    var result: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add((blank + prev[i]!!) + blank); _tmp }
        i = i + 1
    }
    i = 0
    while (i < size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add((prev[i]!! + prev[i]!!) + prev[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add((blank + prev[i]!!) + blank); _tmp }
        i = i + 1
    }
    return result
}

fun print_pattern(pattern: MutableList<String>): Unit {
    var i: Int = (0).toInt()
    while (i < pattern.size) {
        println(pattern[i]!!)
        i = i + 1
    }
}

fun user_main(): Unit {
    var depth: Int = (3).toInt()
    var pattern: MutableList<String> = vicsek(depth)
    print_pattern(pattern)
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
