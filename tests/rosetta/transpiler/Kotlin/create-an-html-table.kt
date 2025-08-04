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

fun user_main(): Unit {
    var rows: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (i in 0 until 4) {
        rows = run { val _tmp = rows.toMutableList(); _tmp.add(mutableListOf(i * 3, (i * 3) + 1, (i * 3) + 2)); _tmp }
    }
    println("<table>")
    println("    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>")
    var idx: Int = 0
    for (row in rows) {
        println(((((((("    <tr><td>" + idx.toString()) + "</td><td>") + (row[0]!!).toString()) + "</td><td>") + (row[1]!!).toString()) + "</td><td>") + (row[2]!!).toString()) + "</td></tr>")
        idx = idx + 1
    }
    println("</table>")
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
