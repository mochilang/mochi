import java.math.BigInteger

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
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

fun is_bit_set(number: Int, position: Int): Boolean {
    var shifted: Int = (((number).toLong() / pow2(position)).toInt())
    var remainder: Int = Math.floorMod(shifted, 2)
    return remainder == 1
}

fun set_bit(number: Int, position: Int): Int {
    if (((is_bit_set(number, position)) as Boolean)) {
        return number
    }
    return (((number).toLong() + pow2(position)).toInt())
}

fun clear_bit(number: Int, position: Int): Int {
    if (((is_bit_set(number, position)) as Boolean)) {
        return (((number).toLong() - pow2(position)).toInt())
    }
    return number
}

fun flip_bit(number: Int, position: Int): Int {
    if (((is_bit_set(number, position)) as Boolean)) {
        return (((number).toLong() - pow2(position)).toInt())
    }
    return (((number).toLong() + pow2(position)).toInt())
}

fun get_bit(number: Int, position: Int): Int {
    if (((is_bit_set(number, position)) as Boolean)) {
        return 1
    }
    return 0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(set_bit(13, 1).toString())
        println(clear_bit(18, 1).toString())
        println(flip_bit(5, 1).toString())
        println(is_bit_set(10, 3).toString())
        println(get_bit(10, 1).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
