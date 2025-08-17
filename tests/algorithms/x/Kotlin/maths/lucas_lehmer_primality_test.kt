import java.math.BigInteger

fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

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

fun lucas_lehmer_test(p: Int): Boolean {
    if (p < 2) {
        panic("p should not be less than 2!")
    }
    if (p == 2) {
        return true
    }
    var s: Int = (4).toInt()
    var m: Int = (pow2(p) - 1).toInt()
    var i: Int = (0).toInt()
    while (i < (p - 2)) {
        s = (Math.floorMod((((s).toLong() * (s).toLong()) - (2).toLong()), (m).toLong())).toInt()
        i = i + 1
    }
    return s == 0
}

fun user_main(): Unit {
    println(lucas_lehmer_test(7).toString())
    println(lucas_lehmer_test(11).toString())
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
