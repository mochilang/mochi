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

fun pow10(n: Int): Double {
    var r: Double = 1.0
    var i: Int = 0
    while (i < n) {
        r = r * 10.0
        i = i + 1
    }
    return r
}

fun formatFloat(f: Double, prec: Int): String {
    var scale: Double = pow10(prec)
    var scaled: Double = (f * scale) + 0.5
    var n: Int = scaled.toInt()
    var digits: String = n.toString()
    while (digits.length <= prec) {
        digits = "0" + digits
    }
    var intPart: String = digits.substring(0, digits.length - prec)
    var fracPart: String = digits.substring(digits.length - prec, digits.length)
    return (intPart + ".") + fracPart
}

fun padLeftZeros(s: String, width: Int): String {
    var out: String = s
    while (out.length < width) {
        out = "0" + out
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(padLeftZeros(formatFloat(7.125, 3), 9))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
