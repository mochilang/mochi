var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

fun poly(p: Int): String {
    var s: String = ""
    var coef: Int = 1
    var i: Int = p
    if (coef != 1) {
        s = s + coef.toString()
    }
    while (i > 0) {
        s = s + "x"
        if (i != 1) {
            s = (s + "^") + i.toString()
        }
        coef = ((coef * i) / ((p - i) + 1)).toInt()
        var d: Int = coef
        if (((p - (i - 1)) % 2) == 1) {
            d = 0 - d
        }
        if (d < 0) {
            s = (s + " - ") + (0 - d).toString()
        } else {
            s = (s + " + ") + d.toString()
        }
        i = i - 1
    }
    if (s == "") {
        s = "1"
    }
    return s
}

fun aks(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    var c: Int = n
    var i: Int = 1
    while (i < n) {
        if ((c % n) != 0) {
            return false
        }
        c = ((c * (n - i)) / (i + 1)).toInt()
        i = i + 1
    }
    return true
}

fun user_main(): Unit {
    var p: Int = 0
    while (p <= 7) {
        println((p.toString() + ":  ") + poly(p))
        p = p + 1
    }
    var first: Boolean = true
    p = 2
    var line: String = ""
    while (p < 50) {
        if ((aks(p)) as Boolean) {
            if (first as Boolean) {
                line = line + p.toString()
                first = false
            } else {
                line = (line + " ") + p.toString()
            }
        }
        p = p + 1
    }
    println(line)
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
