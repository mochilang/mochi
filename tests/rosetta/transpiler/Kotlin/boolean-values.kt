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

fun parseBool(s: String): Boolean {
    var l: String = s.toLowerCase()
    if ((((((((l == "1") || (l == "t") as Boolean)) || (l == "true") as Boolean)) || (l == "yes") as Boolean)) || (l == "y")) {
        return true
    }
    return false
}

fun user_main(): Unit {
    var n: Boolean = true
    println(n)
    println("bool")
    n = (!n as Boolean)
    println(n)
    var x: Int = 5
    var y: Int = 8
    println(listOf("x == y:", x == y).joinToString(" "))
    println(listOf("x < y:", x < y).joinToString(" "))
    println("\nConvert String into Boolean Data type\n")
    var str1: String = "japan"
    println(listOf("Before :", "string").joinToString(" "))
    var bolStr: Boolean = parseBool(str1)
    println(listOf("After :", "bool").joinToString(" "))
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
