import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var K: Double = 8987551792.3
fun format2(x: Double): String {
    var sign: String = (if (x < 0.0) "-" else "" as String)
    var y: Double = (if (x < 0.0) 0.0 - x else x.toDouble())
    var m: Double = 100.0
    var scaled: Double = y * m
    var i: Int = ((scaled.toInt())).toInt()
    if ((scaled - ((i.toDouble()))) >= 0.5) {
        i = i + 1
    }
    var int_part: Int = (i / 100).toInt()
    var frac_part: Int = (Math.floorMod(i, 100)).toInt()
    var frac_str: String = frac_part.toString()
    if (frac_part < 10) {
        frac_str = "0" + frac_str
    }
    return ((sign + int_part.toString()) + ".") + frac_str
}

fun coulombs_law(q1: Double, q2: Double, radius: Double): Double {
    if (radius <= 0.0) {
        panic("radius must be positive")
    }
    var force: Double = ((K * q1) * q2) / (radius * radius)
    return force
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(format2(coulombs_law(15.5, 20.0, 15.0)))
        println(format2(coulombs_law(1.0, 15.0, 5.0)))
        println(format2(coulombs_law(20.0, 0.0 - 50.0, 15.0)))
        println(format2(coulombs_law(0.0 - 5.0, 0.0 - 8.0, 10.0)))
        println(format2(coulombs_law(50.0, 100.0, 50.0)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
