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

fun countDivisors(n: Int): Int {
    if (n < 2) {
        return 1
    }
    var count: Int = 2
    var i: Int = 2
    while (i <= (n / 2)) {
        if ((n % i) == 0) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun user_main(): Unit {
    println("The first 20 anti-primes are:")
    var maxDiv: Int = 0
    var count: Int = 0
    var n: Int = 1
    var line: String = ""
    while (count < 20) {
        val d: Int = countDivisors(n)
        if (d > maxDiv) {
            line = (line + n.toString()) + " "
            maxDiv = d
            count = count + 1
        }
        n = n + 1
    }
    line = line.substring(0, line.length - 1) as String
    println(line)
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
