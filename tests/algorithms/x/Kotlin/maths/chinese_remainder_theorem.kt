import java.math.BigInteger

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

data class EuclidResult(var x: Int = 0, var y: Int = 0)
var e1: EuclidResult = extended_euclid(10, 6)
fun extended_euclid(a: Int, b: Int): EuclidResult {
    if (b == 0) {
        return EuclidResult(x = 1, y = 0)
    }
    var res: EuclidResult = extended_euclid(b, Math.floorMod(a, b))
    var k: Int = (a / b).toInt()
    return EuclidResult(x = res.y, y = res.x - (k * res.y))
}

fun chinese_remainder_theorem(n1: Int, r1: Int, n2: Int, r2: Int): Int {
    var res: EuclidResult = extended_euclid(n1, n2)
    var x: Int = (res.x).toInt()
    var y: Int = (res.y).toInt()
    var m: Int = (n1 * n2).toInt()
    var n: Int = (((r2 * x) * n1) + ((r1 * y) * n2)).toInt()
    return Math.floorMod(((Math.floorMod(n, m)) + m), m)
}

fun invert_modulo(a: Int, n: Int): Int {
    var res: EuclidResult = extended_euclid(a, n)
    var b: Int = (res.x).toInt()
    if (b < 0) {
        b = Math.floorMod(((Math.floorMod(b, n)) + n), n)
    }
    return b
}

fun chinese_remainder_theorem2(n1: Int, r1: Int, n2: Int, r2: Int): Int {
    var x: Int = (invert_modulo(n1, n2)).toInt()
    var y: Int = (invert_modulo(n2, n1)).toInt()
    var m: Int = (n1 * n2).toInt()
    var n: Int = (((r2 * x) * n1) + ((r1 * y) * n2)).toInt()
    return Math.floorMod(((Math.floorMod(n, m)) + m), m)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((e1.x.toString() + ",") + e1.y.toString())
        var e2: EuclidResult = extended_euclid(7, 5)
        println((e2.x.toString() + ",") + e2.y.toString())
        println(chinese_remainder_theorem(5, 1, 7, 3).toString())
        println(chinese_remainder_theorem(6, 1, 4, 3).toString())
        println(invert_modulo(2, 5).toString())
        println(invert_modulo(8, 7).toString())
        println(chinese_remainder_theorem2(5, 1, 7, 3).toString())
        println(chinese_remainder_theorem2(6, 1, 4, 3).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
