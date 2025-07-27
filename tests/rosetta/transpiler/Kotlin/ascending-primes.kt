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

var asc: MutableList<Int> = mutableListOf()
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

fun gen(first: Int, cand: Int, digits: Int): Unit {
    if (digits == 0) {
        if ((isPrime(cand)) as Boolean) {
            asc = (asc + mutableListOf(cand)).toMutableList()
        }
        return
    }
    var i: Int = first
    while (i < 10) {
        gen(i + 1, (cand * 10) + i, digits - 1)
        i = (i + 1)
    }
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    var digits: Int = 1
    while (digits < 10) {
        gen(1, 0, digits)
        digits = digits + 1
    }
    println(("There are " + asc.size.toString()) + " ascending primes, namely:")
    var i: Int = 0
    var line: String = ""
    while (i < asc.size) {
        line = (line + pad(asc[i], 8)) + " "
        if (((i + 1) % 10) == 0) {
            println(line.substring(0, line.length - 1))
            line = ""
        }
        i = i + 1
    }
    if (line.length > 0) {
        println(line.substring(0, line.length - 1))
    }
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
