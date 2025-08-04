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

fun damm(s: String): Boolean {
    var tbl: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 3, 1, 7, 5, 9, 8, 6, 4, 2), mutableListOf(7, 0, 9, 2, 1, 5, 4, 8, 6, 3), mutableListOf(4, 2, 0, 6, 8, 7, 1, 3, 5, 9), mutableListOf(1, 7, 5, 0, 9, 8, 3, 4, 2, 6), mutableListOf(6, 1, 2, 3, 0, 4, 5, 9, 7, 8), mutableListOf(3, 6, 7, 4, 2, 0, 9, 5, 8, 1), mutableListOf(5, 8, 6, 9, 7, 2, 0, 1, 3, 4), mutableListOf(8, 9, 4, 5, 3, 6, 2, 0, 1, 7), mutableListOf(9, 4, 3, 8, 6, 1, 7, 2, 0, 5), mutableListOf(2, 5, 8, 1, 4, 3, 6, 7, 9, 0))
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    var interim: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var digit: Int = (((digits)[s.substring(i, i + 1)]!!).toInt())
        var row: MutableList<Int> = tbl[interim]!!
        interim = row[digit]!!
        i = i + 1
    }
    return interim == 0
}

fun padLeft(s: String, width: Int): String {
    var s: String = s
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    for (s in mutableListOf("5724", "5727", "112946", "112949")) {
        println((padLeft(s, 6) + "  ") + damm(s).toString())
    }
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
