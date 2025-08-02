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

fun quibble(items: MutableList<String>): String {
    var n: Int = items.size
    if (n == 0) {
        return "{}"
    } else {
        if (n == 1) {
            return ("{" + items[0]!!) + "}"
        } else {
            if (n == 2) {
                return ((("{" + items[0]!!) + " and ") + items[1]!!) + "}"
            } else {
                var prefix: String = ""
                for (i in 0 until n - 1) {
                    if (i == (n - 1)) {
                        break
                    }
                    if (i > 0) {
                        prefix = prefix + ", "
                    }
                    prefix = prefix + items[i]!!
                }
                return ((("{" + prefix) + " and ") + items[n - 1]!!) + "}"
            }
        }
    }
}

fun user_main(): Unit {
    println(quibble(mutableListOf<String>()))
    println(quibble(mutableListOf("ABC")))
    println(quibble(mutableListOf("ABC", "DEF")))
    println(quibble(mutableListOf("ABC", "DEF", "G", "H")))
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
