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

fun gcd(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    while (y != 0) {
        var t: Int = (Math.floorMod(x, y)).toInt()
        x = y
        y = t
    }
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun proper_fractions(den: Int): MutableList<String> {
    if (den < 0) {
        panic("The Denominator Cannot be less than 0")
    }
    var res: MutableList<String> = mutableListOf<String>()
    var n: Int = (1).toInt()
    while (n < den) {
        if (gcd(n, den) == 1) {
            res = run { val _tmp = res.toMutableList(); _tmp.add((_numToStr(n) + "/") + _numToStr(den)); _tmp }
        }
        n = n + 1
    }
    return res
}

fun test_proper_fractions(): Unit {
    var a: MutableList<String> = proper_fractions(10)
    if (a != mutableListOf("1/10", "3/10", "7/10", "9/10")) {
        panic("test 10 failed")
    }
    var b: MutableList<String> = proper_fractions(5)
    if (b != mutableListOf("1/5", "2/5", "3/5", "4/5")) {
        panic("test 5 failed")
    }
    var c: MutableList<String> = proper_fractions(0)
    if (c != mutableListOf<Any?>()) {
        panic("test 0 failed")
    }
}

fun user_main(): Unit {
    test_proper_fractions()
    println(proper_fractions(10).toString())
    println(proper_fractions(5).toString())
    println(proper_fractions(0).toString())
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
