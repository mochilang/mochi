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

var MAX: Long = 4294967296L
var HALF: Long = 2147483648L
fun to_unsigned(n: Int): Int {
    if (n < 0) {
        return ((MAX + (n).toLong()).toInt())
    }
    return n
}

fun from_unsigned(n: Int): Int {
    if (n >= HALF) {
        return (((n).toLong() - MAX).toInt())
    }
    return n
}

fun bit_and(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    var res: Int = (0).toInt()
    var bit: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < 32) {
        if (((Math.floorMod(x, 2)) == 1) && ((Math.floorMod(y, 2)) == 1)) {
            res = res + bit
        }
        x = x / 2
        y = y / 2
        bit = bit * 2
        i = i + 1
    }
    return res
}

fun bit_xor(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    var res: Int = (0).toInt()
    var bit: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < 32) {
        var abit: Int = (Math.floorMod(x, 2)).toInt()
        var bbit: Int = (Math.floorMod(y, 2)).toInt()
        if ((Math.floorMod((abit + bbit), 2)) == 1) {
            res = res + bit
        }
        x = x / 2
        y = y / 2
        bit = bit * 2
        i = i + 1
    }
    return res
}

fun lshift1(num: Int): Int {
    return ((Math.floorMod(((num * 2).toLong()), MAX)).toInt())
}

fun add(a: Int, b: Int): Int {
    var first: Int = (to_unsigned(a)).toInt()
    var second: Int = (to_unsigned(b)).toInt()
    while (second != 0) {
        var carry: Int = (bit_and(first, second)).toInt()
        first = bit_xor(first, second)
        second = lshift1(carry)
    }
    var result: Int = (from_unsigned(first)).toInt()
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(add(3, 5).toString())
        println(add(13, 5).toString())
        println(add(0 - 7, 2).toString())
        println(add(0, 0 - 7).toString())
        println(add(0 - 321, 0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
