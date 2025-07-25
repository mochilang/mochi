var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((n % 2) == 0) {
        return n == 2
    }
    if ((n % 3) == 0) {
        return n == 3
    }
    var d: Int = 5
    while ((d * d) <= n) {
        if ((n % d) == 0) {
            return false
        }
        d = d + 2
        if ((n % d) == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

fun sumDigits(n: Int): Int {
    var s: Int = 0
    var x: Int = n
    while (x > 0) {
        s = s + (x % 10)
        x = (x / 10).toInt()
    }
    return s
}

fun pad(n: Int): String {
    if (n < 10) {
        return "  " + n.toString()
    }
    if (n < 100) {
        return " " + n.toString()
    }
    return n.toString()
}

fun user_main(): Unit {
    println("Additive primes less than 500:")
    var count: Int = 0
    var line: String = ""
    var lineCount: Int = 0
    var i: Int = 2
    while (i < 500) {
        if (isPrime(i) && isPrime(sumDigits(i))) {
            count = count + 1
            line = (line + pad(i)) + "  "
            lineCount = lineCount + 1
            if (lineCount == 10) {
                println(line.substring(0, line.length - 2))
                line = ""
                lineCount = 0
            }
        }
        if (i > 2) {
            i = i + 2
        } else {
            i = i + 1
        }
    }
    if (lineCount > 0) {
        println(line.substring(0, line.length - 2))
    }
    println(count.toString() + " additive primes found.")
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
