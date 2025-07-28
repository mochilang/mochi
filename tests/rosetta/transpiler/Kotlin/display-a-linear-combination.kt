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

fun padRight(s: String, w: Int): String {
    var r: String = s
    while (r.length < w) {
        r = r + " "
    }
    return r
}

fun linearCombo(c: MutableList<Int>): String {
    var out: String = ""
    var i: Int = 0
    while (i < c.size) {
        val n: Int = c[i]
        if (n != 0) {
            var op: String = ""
            if ((n < 0) && (out.length == 0)) {
                op = "-"
            } else {
                if (n < 0) {
                    op = " - "
                } else {
                    if ((n > 0) && (out.length == 0)) {
                        op = ""
                    } else {
                        op = " + "
                    }
                }
            }
            var av: Int = n
            if (av < 0) {
                av = 0 - av
            }
            var coeff: String = av.toString() + "*"
            if (av == 1) {
                coeff = ""
            }
            out = ((((out + op) + coeff) + "e(") + (i + 1).toString()) + ")"
        }
        i = i + 1
    }
    if (out.length == 0) {
        return "0"
    }
    return out
}

fun user_main(): Unit {
    val combos: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3), mutableListOf(0, 1, 2, 3), mutableListOf(1, 0, 3, 4), mutableListOf(1, 2, 0), mutableListOf(0, 0, 0), mutableListOf(0), mutableListOf(1, 1, 1), mutableListOf(0 - 1, 0 - 1, 0 - 1), mutableListOf(0 - 1, 0 - 2, 0, 0 - 3), mutableListOf(0 - 1))
    var idx: Int = 0
    while (idx < combos.size) {
        val c: MutableList<Int> = combos[idx]
        var t: String = "["
        var j: Int = 0
        while (j < c.size) {
            t = t + (c[j]).toString()
            if (j < (c.size - 1)) {
                t = t + ", "
            }
            j = j + 1
        }
        t = t + "]"
        val lc: String = linearCombo(c)
        println((padRight(t, 15) + "  ->  ") + lc)
        idx = idx + 1
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
