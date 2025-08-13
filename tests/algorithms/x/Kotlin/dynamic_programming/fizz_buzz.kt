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

fun fizz_buzz(number: Int, iterations: Int): String {
    if (number < 1) {
        panic("starting number must be an integer and be more than 0")
    }
    if (iterations < 1) {
        panic("Iterations must be done more than 0 times to play FizzBuzz")
    }
    var out: String = ""
    var n: Int = (number).toInt()
    while (n <= iterations) {
        if ((Math.floorMod(n, 3)) == 0) {
            out = out + "Fizz"
        }
        if ((Math.floorMod(n, 5)) == 0) {
            out = out + "Buzz"
        }
        if (((Math.floorMod(n, 3)) != 0) && ((Math.floorMod(n, 5)) != 0)) {
            out = out + n.toString()
        }
        out = out + " "
        n = n + 1
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(fizz_buzz(1, 7))
        println(fizz_buzz(1, 15))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
