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

var s1: MutableList<Int> = diophantine(10, 6, 14)
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

fun extended_gcd(a: Int, b: Int): MutableList<Int> {
    if (b == 0) {
        return mutableListOf(a, 1, 0)
    }
    var res: MutableList<Int> = extended_gcd(b, Math.floorMod(a, b))
    var d: Int = res[0]!!
    var p: Int = res[1]!!
    var q: Int = res[2]!!
    var x: Int = q
    var y: Int = p - (q * (a / b))
    return mutableListOf(d, x, y)
}

fun diophantine(a: Int, b: Int, c: Int): MutableList<Int> {
    var d: Int = gcd(a, b)
    if ((Math.floorMod(c, d)) != 0) {
        panic("No solution")
    }
    var eg: MutableList<Int> = extended_gcd(a, b)
    var r: Int = c / d
    var x: Int = eg[1]!! * r
    var y: Int = eg[2]!! * r
    return mutableListOf(x, y)
}

fun diophantine_all_soln(a: Int, b: Int, c: Int, n: Int): MutableList<MutableList<Int>> {
    var base: MutableList<Int> = diophantine(a, b, c)
    var x0: Int = base[0]!!
    var y0: Int = base[1]!!
    var d: Int = gcd(a, b)
    var p: Int = a / d
    var q: Int = b / d
    var sols: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < n) {
        var x: Int = x0 + (i * q)
        var y: Int = y0 - (i * p)
        sols = run { val _tmp = sols.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp }
        i = i + 1
    }
    return sols
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(s1.toString())
        var sols: MutableList<MutableList<Int>> = diophantine_all_soln(10, 6, 14, 4)
        var j: Int = 0
        while (j < sols.size) {
            println((sols[j]!!).toString())
            j = j + 1
        }
        println(diophantine(391, 299, 0 - 69).toString())
        println(extended_gcd(10, 6).toString())
        println(extended_gcd(7, 5).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
