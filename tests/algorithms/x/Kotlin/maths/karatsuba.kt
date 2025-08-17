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

fun int_pow(base: Int, exp: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun karatsuba(a: Int, b: Int): Int {
    if ((_numToStr(a).length == 1) || (_numToStr(b).length == 1)) {
        return a * b
    }
    var m1: Int = (_numToStr(a).length).toInt()
    var lb: Int = (_numToStr(b).length).toInt()
    if (lb > m1) {
        m1 = lb
    }
    var m2: Int = (m1 / 2).toInt()
    var power: Int = (int_pow(10, m2)).toInt()
    var a1: Int = (a / power).toInt()
    var a2: Int = (Math.floorMod(a, power)).toInt()
    var b1: Int = (b / power).toInt()
    var b2: Int = (Math.floorMod(b, power)).toInt()
    var x: Int = (karatsuba(a2, b2)).toInt()
    var y: Int = (karatsuba(a1 + a2, b1 + b2)).toInt()
    var z: Int = (karatsuba(a1, b1)).toInt()
    var result: Int = (((z * int_pow(10, 2 * m2)) + (((y - z) - x) * power)) + x).toInt()
    return result
}

fun user_main(): Unit {
    println(_numToStr(karatsuba(15463, 23489)))
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
