import java.math.BigInteger

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

var PI: Double = 3.141592653589793
fun floor(x: Double): Double {
    var i: Int = (x.toInt()).toInt()
    if ((i.toDouble()) > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun pow(x: Double, n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * x
        i = i + 1
    }
    return result
}

fun factorial(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (2).toInt()
    while (i <= n) {
        result = result * (i.toDouble())
        i = i + 1
    }
    return result
}

fun maclaurin_sin(theta: Double, accuracy: Int): Double {
    var t: Double = theta
    var div: Double = floor(t / (2.0 * PI))
    t = t - ((2.0 * div) * PI)
    var sum: Double = 0.0
    var r: Int = (0).toInt()
    while (r < accuracy) {
        var power: Int = ((2 * r) + 1).toInt()
        var sign: Double = if ((Math.floorMod(r, 2)) == 0) 1.0 else 0.0 - 1.0.toDouble()
        sum = sum + ((sign * pow(t, power)) / factorial(power))
        r = r + 1
    }
    return sum
}

fun maclaurin_cos(theta: Double, accuracy: Int): Double {
    var t: Double = theta
    var div: Double = floor(t / (2.0 * PI))
    t = t - ((2.0 * div) * PI)
    var sum: Double = 0.0
    var r: Int = (0).toInt()
    while (r < accuracy) {
        var power: Int = (2 * r).toInt()
        var sign: Double = if ((Math.floorMod(r, 2)) == 0) 1.0 else 0.0 - 1.0.toDouble()
        sum = sum + ((sign * pow(t, power)) / factorial(power))
        r = r + 1
    }
    return sum
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(maclaurin_sin(10.0, 30)))
        println(_numToStr(maclaurin_sin(0.0 - 10.0, 30)))
        println(_numToStr(maclaurin_sin(10.0, 15)))
        println(_numToStr(maclaurin_sin(0.0 - 10.0, 15)))
        println(_numToStr(maclaurin_cos(5.0, 30)))
        println(_numToStr(maclaurin_cos(0.0 - 5.0, 30)))
        println(_numToStr(maclaurin_cos(10.0, 15)))
        println(_numToStr(maclaurin_cos(0.0 - 10.0, 15)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
