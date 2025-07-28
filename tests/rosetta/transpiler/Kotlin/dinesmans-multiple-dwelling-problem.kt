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

fun absInt(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun user_main(): Unit {
    var b: Int = 1
    while (b <= 5) {
        if (b != 5) {
            var c: Int = 1
            while (c <= 5) {
                if ((c != 1) && (c != b)) {
                    var f: Int = 1
                    while (f <= 5) {
                        if ((((((((f != 1) && (f != 5) as Boolean)) && (f != b) as Boolean)) && (f != c) as Boolean)) && (absInt(f - c) > 1)) {
                            var m: Int = 1
                            while (m <= 5) {
                                if ((((((m != b) && (m != c) as Boolean)) && (m != f) as Boolean)) && (m > c)) {
                                    var s: Int = 1
                                    while (s <= 5) {
                                        if ((((((((s != b) && (s != c) as Boolean)) && (s != f) as Boolean)) && (s != m) as Boolean)) && (absInt(s - f) > 1)) {
                                            println(((((((((("Baker in " + b.toString()) + ", Cooper in ") + c.toString()) + ", Fletcher in ") + f.toString()) + ", Miller in ") + m.toString()) + ", Smith in ") + s.toString()) + ".")
                                            return
                                        }
                                        s = s + 1
                                    }
                                }
                                m = m + 1
                            }
                        }
                        f = f + 1
                    }
                }
                c = c + 1
            }
        }
        b = b + 1
    }
    println("No solution found.")
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
