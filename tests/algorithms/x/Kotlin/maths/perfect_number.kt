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

fun perfect(n: Int): Boolean {
    if (n <= 0) {
        return false
    }
    var total: Int = (0).toInt()
    var divisor: Int = (1).toInt()
    while (divisor <= (n / 2)) {
        if ((Math.floorMod(n, divisor)) == 0) {
            total = total + divisor
        }
        divisor = divisor + 1
    }
    return total == n
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(perfect(27).toString())
        println(perfect(28).toString())
        println(perfect(29).toString())
        println(perfect(6).toString())
        println(perfect(12).toString())
        println(perfect(496).toString())
        println(perfect(8128).toString())
        println(perfect(0).toString())
        println(perfect(0 - 1).toString())
        println(perfect(33550336).toString())
        println(perfect(33550337).toString())
        println(perfect(1).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
