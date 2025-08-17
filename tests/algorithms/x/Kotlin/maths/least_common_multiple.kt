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

fun gcd(a: Int, b: Int): Int {
    var x: Int = (if (a >= 0) a else 0 - a.toInt()).toInt()
    var y: Int = (if (b >= 0) b else 0 - b.toInt()).toInt()
    while (y != 0) {
        var temp: Int = (Math.floorMod(x, y)).toInt()
        x = y
        y = temp
    }
    return x
}

fun lcm_slow(a: Int, b: Int): Int {
    var max: Int = (if (a >= b) a else b).toInt()
    var multiple: Int = (max).toInt()
    while (((Math.floorMod(multiple, a)) != 0) || ((Math.floorMod(multiple, b)) != 0)) {
        multiple = multiple + max
    }
    return multiple
}

fun lcm_fast(a: Int, b: Int): Int {
    return (a / gcd(a, b)) * b
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(lcm_slow(5, 2)))
        println(_numToStr(lcm_slow(12, 76)))
        println(_numToStr(lcm_fast(5, 2)))
        println(_numToStr(lcm_fast(12, 76)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
