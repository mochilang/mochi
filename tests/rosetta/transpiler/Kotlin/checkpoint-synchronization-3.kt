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

var partList: MutableList<String> = mutableListOf("A", "B", "C", "D")
var nAssemblies: Int = 3
fun lower(ch: String): String {
    var up: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var low: String = "abcdefghijklmnopqrstuvwxyz"
    var i: Int = 0
    while (i < up.length) {
        if (ch == up.substring(i, i + 1)) {
            return low.substring(i, i + 1)
        }
        i = i + 1
    }
    return ch
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (p in partList) {
            println(p + " worker running")
        }
        for (cycle in 1 until nAssemblies + 1) {
            println("begin assembly cycle " + cycle.toString())
            var a: String = ""
            for (p in partList) {
                println(p + " worker begins part")
                println((p + " worker completed ") + (p.toLowerCase()).toString())
                a = a + (p.toLowerCase()).toString()
            }
            println(((a + " assembled.  cycle ") + cycle.toString()) + " complete")
        }
        for (p in partList) {
            println(p + " worker stopped")
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
