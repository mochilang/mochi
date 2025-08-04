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
    var x: Int = a
    if (x < 0) {
        x = 0 - x
    }
    var y: Int = b
    if (y < 0) {
        y = 0 - y
    }
    while (y != 0) {
        var t: BigInteger = (Math.floorMod(x, y)).toBigInteger()
        x = y
        y = (t.toInt())
    }
    return x
}

fun parseRational(s: String): MutableMap<String, Int> {
    var intPart: Int = 0
    var fracPart: Int = 0
    var denom: Int = 1
    var afterDot: Boolean = false
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == ".") {
            afterDot = true
        } else {
            var d: BigInteger = (((ch.toInt())) - (("0".toInt()))).toBigInteger()
            if (!afterDot) {
                intPart = ((((intPart * 10)).toBigInteger().add((d))).toInt())
            } else {
                fracPart = ((((fracPart * 10)).toBigInteger().add((d))).toInt())
                denom = denom * 10
            }
        }
        i = i + 1
    }
    var num: Int = (intPart * denom) + fracPart
    var g: Int = gcd(num, denom)
    return mutableMapOf<String, Int>("num" to (((num / g).toInt())), "den" to (((denom / g).toInt())))
}

fun user_main(): Unit {
    var inputs: MutableList<String> = mutableListOf("0.9054054", "0.518518", "0.75")
    for (s in inputs) {
        var r: MutableMap<String, Int> = parseRational(s)
        println((((s + " = ") + ((r)["num"]!!).toString()) + "/") + ((r)["den"]!!).toString())
    }
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
