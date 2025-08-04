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

var digits: MutableList<Int> = mutableListOf(9, 8, 7, 6, 5, 4, 3, 2, 1)
var primes: MutableList<Int> = gen(0, 0, false)
fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return n == 2
    }
    var d: Int = 3
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 2
    }
    return true
}

fun gen(idx: Int, cur: Int, used: Boolean): MutableList<Int> {
    if (idx == digits.size) {
        if (used && isPrime(cur)) {
            return mutableListOf(cur)
        }
        return mutableListOf<Int>()
    }
    var with: MutableList<Int> = gen(idx + 1, (cur * 10) + digits[idx]!!, true)
    var without: MutableList<Int> = gen(idx + 1, cur, used)
    return (with + without).toMutableList()
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(("There are " + primes.size.toString()) + " descending primes, namely:")
        var i: Int = 0
        var line: String = ""
        while (i < primes.size) {
            line = (line + pad(primes[i]!!, 8)) + " "
            if ((Math.floorMod((i + 1), 10)) == 0) {
                println(line.substring(0, line.length - 1))
                line = ""
            }
            i = i + 1
        }
        if (line.length > 0) {
            println(line.substring(0, line.length - 1))
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
