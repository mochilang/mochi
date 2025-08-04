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

var candidates: MutableList<String> = mutableListOf("037833100", "17275R102", "38259P508", "594918104", "68389X106", "68389X105")
fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    if ((ch >= "0") && (ch <= "9")) {
        return Integer.parseInt(ch, 10) + 48
    }
    var idx: Int = upper.indexOf(ch)
    if (idx >= 0) {
        return 65 + idx
    }
    return 0
}

fun isCusip(s: String): Boolean {
    if (s.length != 9) {
        return false
    }
    var sum: Int = 0
    var i: Int = 0
    while (i < 8) {
        var c: String = s.substring(i, i + 1)
        var v: Int = 0
        if ((c >= "0") && (c <= "9")) {
            v = ((Integer.parseInt(c, 10)) as Int)
        } else {
            if ((c >= "A") && (c <= "Z")) {
                v = ord(c) - 55
            } else {
                if (c == "*") {
                    v = 36
                } else {
                    if (c == "@") {
                        v = 37
                    } else {
                        if (c == "#") {
                            v = 38
                        } else {
                            return false
                        }
                    }
                }
            }
        }
        if ((Math.floorMod(i, 2)) == 1) {
            v = v * 2
        }
        sum = (sum + (v / 10)) + (Math.floorMod(v, 10))
        i = i + 1
    }
    return Integer.parseInt(s.substring(8, 9), 10) == (Math.floorMod((10 - (Math.floorMod(sum, 10))), 10))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (cand in candidates) {
            var b: String = "incorrect"
            if (((isCusip(cand)) as Boolean)) {
                b = "correct"
            }
            println((cand + " -> ") + b)
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
