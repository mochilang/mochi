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

fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return n == 2
    }
    if ((Math.floorMod(n, 3)) == 0) {
        return n == 3
    }
    var d: Int = 5
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 2
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

fun countPrimeFactors(n: Int): Int {
    var n: Int = n
    if (n == 1) {
        return 0
    }
    if ((isPrime(n)) as Boolean) {
        return 1
    }
    var count: Int = 0
    var f: Int = 2
    while (true) {
        if ((Math.floorMod(n, f)) == 0) {
            count = count + 1
            n = n / f
            if (n == 1) {
                return count
            }
            if ((isPrime(n)) as Boolean) {
                f = n
            }
        } else {
            if (f >= 3) {
                f = f + 2
            } else {
                f = 3
            }
        }
    }
    return count
}

fun pad4(n: Int): String {
    var s: String = n.toString()
    while (s.length < 4) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    val max: Int = 120
    println(("The attractive numbers up to and including " + max.toString()) + " are:")
    var count: Int = 0
    var line: String = ""
    var lineCount: Int = 0
    var i: Int = 1
    while (i <= max) {
        val c: Int = countPrimeFactors(i)
        if ((isPrime(c)) as Boolean) {
            line = line + pad4(i)
            count = count + 1
            lineCount = lineCount + 1
            if (lineCount == 20) {
                println(line)
                line = ""
                lineCount = 0
            }
        }
        i = i + 1
    }
    if (lineCount > 0) {
        println(line)
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
