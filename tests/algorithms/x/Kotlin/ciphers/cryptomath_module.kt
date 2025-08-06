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

fun gcd(a: Int, b: Int): Int {
    var x: Int = (if (a < 0) 0 - a else a as Int)
    var y: Int = (if (b < 0) 0 - b else b as Int)
    while (y != 0) {
        var t: Int = Math.floorMod(x, y)
        x = y
        y = t
    }
    return x
}

fun find_mod_inverse(a: Int, m: Int): Int {
    if (gcd(a, m) != 1) {
        error(((("mod inverse of " + a.toString()) + " and ") + m.toString()) + " does not exist")
    }
    var u1: Int = 1
    var u2: Int = 0
    var u3: Int = a
    var v1: Int = 0
    var v2: Int = 1
    var v3: Int = m
    while (v3 != 0) {
        var q: Int = u3 / v3
        var t1: Int = u1 - (q * v1)
        var t2: Int = u2 - (q * v2)
        var t3: Int = u3 - (q * v3)
        u1 = v1
        u2 = v2
        u3 = v3
        v1 = t1
        v2 = t2
        v3 = t3
    }
    var res: Int = Math.floorMod(u1, m)
    if (res < 0) {
        res = res + m
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_mod_inverse(3, 11).toString())
        println(find_mod_inverse(7, 26).toString())
        println(find_mod_inverse(11, 26).toString())
        println(find_mod_inverse(17, 43).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
