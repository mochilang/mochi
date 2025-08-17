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

fun prime_factors(n: Int): MutableList<Int> {
    var i: Int = (2).toInt()
    var x: Int = (n).toInt()
    var factors: MutableList<Int> = mutableListOf<Int>()
    while (((i).toLong() * (i).toLong()) <= x) {
        if ((Math.floorMod(x, i)) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(i); _tmp }
            x = (x / i).toInt()
        } else {
            i = i + 1
        }
    }
    if (x > 1) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(x); _tmp }
    }
    return factors
}

fun liouville_lambda(n: Int): Int {
    if (n < 1) {
        panic("Input must be a positive integer")
    }
    var cnt: Int = ((prime_factors(n)).size).toInt()
    if ((Math.floorMod(cnt, 2)) == 0) {
        return 1
    }
    return 0 - 1
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(liouville_lambda(10))
        println(liouville_lambda(11))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
