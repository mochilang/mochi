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

data class FibPair(var fn: Int = 0, var fn1: Int = 0)
var i: Int = (0).toInt()
fun _fib(n: Int): FibPair {
    if (n == 0) {
        return FibPair(fn = 0, fn1 = 1)
    }
    var half: FibPair = _fib(n / 2)
    var a: Int = (half.fn).toInt()
    var b: Int = (half.fn1).toInt()
    var c: Int = (a * ((b * 2) - a)).toInt()
    var d: Int = ((a * a) + (b * b)).toInt()
    if ((Math.floorMod(n, 2)) == 0) {
        return FibPair(fn = c, fn1 = d)
    }
    return FibPair(fn = d, fn1 = c + d)
}

fun fibonacci(n: Int): Int {
    if (n < 0) {
        panic("Negative arguments are not supported")
    }
    var res: FibPair = _fib(n)
    return res.fn
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < 13) {
            println(fibonacci(i).toString())
            i = (i + 1).toInt()
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
