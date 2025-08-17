import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun mod(a: Int, n: Int): Int {
    var r: Int = (Math.floorMod(a, n)).toInt()
    if (r < 0) {
        return r + n
    }
    return r
}

fun greatest_common_divisor(a: Int, b: Int): Int {
    var x: Int = (if (a < 0) 0 - a else a.toInt()).toInt()
    var y: Int = (if (b < 0) 0 - b else b.toInt()).toInt()
    while (y != 0) {
        var t: Int = (Math.floorMod(x, y)).toInt()
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
    var d: Int = (res[0]!!).toInt()
    var p: Int = (res[1]!!).toInt()
    var q: Int = (res[2]!!).toInt()
    var x: Int = (q).toInt()
    var y: Int = (p - (q * (a / b))).toInt()
    return mutableListOf(d, x, y)
}

fun extended_euclid(a: Int, b: Int): MutableList<Int> {
    if (b == 0) {
        return mutableListOf(1, 0)
    }
    var res: MutableList<Int> = extended_euclid(b, Math.floorMod(a, b))
    var x: Int = (res[1]!!).toInt()
    var y: Int = (res[0]!! - ((a / b) * res[1]!!)).toInt()
    return mutableListOf(x, y)
}

fun invert_modulo(a: Int, n: Int): Int {
    var res: MutableList<Int> = extended_euclid(a, n)
    var inv: Int = (res[0]!!).toInt()
    return mod(inv, n)
}

fun modular_division(a: Int, b: Int, n: Int): Int {
    if (n <= 1) {
        panic("n must be > 1")
    }
    if (a <= 0) {
        panic("a must be > 0")
    }
    if (greatest_common_divisor(a, n) != 1) {
        panic("gcd(a,n) != 1")
    }
    var eg: MutableList<Int> = extended_gcd(n, a)
    var s: Int = (eg[2]!!).toInt()
    return mod(b * s, n)
}

fun modular_division2(a: Int, b: Int, n: Int): Int {
    var s: Int = (invert_modulo(a, n)).toInt()
    return mod(b * s, n)
}

fun tests(): Unit {
    if (modular_division(4, 8, 5) != 2) {
        panic("md1")
    }
    if (modular_division(3, 8, 5) != 1) {
        panic("md2")
    }
    if (modular_division(4, 11, 5) != 4) {
        panic("md3")
    }
    if (modular_division2(4, 8, 5) != 2) {
        panic("md21")
    }
    if (modular_division2(3, 8, 5) != 1) {
        panic("md22")
    }
    if (modular_division2(4, 11, 5) != 4) {
        panic("md23")
    }
    if (invert_modulo(2, 5) != 3) {
        panic("inv")
    }
    var eg: MutableList<Int> = extended_gcd(10, 6)
    if ((((eg[0]!! != 2) || (eg[1]!! != (0 - 1)) as Boolean)) || (eg[2]!! != 2)) {
        panic("eg")
    }
    var eu: MutableList<Int> = extended_euclid(10, 6)
    if ((eu[0]!! != (0 - 1)) || (eu[1]!! != 2)) {
        panic("eu")
    }
    if (greatest_common_divisor(121, 11) != 11) {
        panic("gcd")
    }
}

fun user_main(): Unit {
    tests()
    println(_numToStr(modular_division(4, 8, 5)))
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
