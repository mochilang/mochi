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

fun index_of(s: String, c: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == c) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun atbash(sequence: String): String {
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower_rev: String = "zyxwvutsrqponmlkjihgfedcba"
    var upper_rev: String = "ZYXWVUTSRQPONMLKJIHGFEDCBA"
    var result: String = ""
    var i: Int = 0
    while (i < sequence.length) {
        var ch: String = sequence[i].toString()
        var idx: Int = index_of(lower, ch)
        if (idx != (0 - 1)) {
            result = result + lower_rev[idx].toString()
        } else {
            var idx2: Int = index_of(upper, ch)
            if (idx2 != (0 - 1)) {
                result = result + upper_rev[idx2].toString()
            } else {
                result = result + ch
            }
        }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(atbash("ABCDEFGH"))
        println(atbash("123GGjj"))
        println(atbash("testStringtest"))
        println(atbash("with space"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
