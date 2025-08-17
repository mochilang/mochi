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

fun is_prime(number: Int): Boolean {
    if (number < 0) {
        panic("is_prime() only accepts positive integers")
    }
    if (number < 2) {
        return false
    }
    if (number < 4) {
        return true
    }
    if (((Math.floorMod(number, 2)) == 0) || ((Math.floorMod(number, 3)) == 0)) {
        return false
    }
    var i: Int = (5).toInt()
    while ((i * i) <= number) {
        if (((Math.floorMod(number, i)) == 0) || ((Math.floorMod(number, (i + 2))) == 0)) {
            return false
        }
        i = i + 6
    }
    return true
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(is_prime(2).toString())
        println(is_prime(3).toString())
        println(is_prime(5).toString())
        println(is_prime(7).toString())
        println(is_prime(11).toString())
        println(is_prime(13).toString())
        println(is_prime(17).toString())
        println(is_prime(19).toString())
        println(is_prime(23).toString())
        println(is_prime(29).toString())
        println(is_prime(0).toString())
        println(is_prime(1).toString())
        println(is_prime(4).toString())
        println(is_prime(6).toString())
        println(is_prime(9).toString())
        println(is_prime(15).toString())
        println(is_prime(105).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
