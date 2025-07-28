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
    var d: Int = 3
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 2
    }
    return true
}

fun revInt(n: Int): Int {
    var r: Int = 0
    var t: Int = n
    while (t > 0) {
        r = (r * 10) + (Math.floorMod(t, 10))
        t = (t / 10).toInt()
    }
    return r
}

fun user_main(): Unit {
    var emirps: MutableList<Int> = mutableListOf<Int>()
    var n: Int = 2
    while (emirps.size < 10000) {
        if ((isPrime(n)) as Boolean) {
            val r: Int = revInt(n)
            if ((r != n) && isPrime(r)) {
                emirps = run { val _tmp = emirps.toMutableList(); _tmp.add(n); _tmp } as MutableList<Int>
            }
        }
        n = n + 1
    }
    var line: String = "   ["
    var i: Int = 0
    while (i < 20) {
        line = line + (emirps[i]).toString()
        if (i < 19) {
            line = line + ", "
        }
        i = i + 1
    }
    line = line + "]"
    println("First 20:")
    println(line)
    line = "  ["
    for (e in emirps) {
        if (e >= 8000) {
            break
        }
        if (e >= 7700) {
            line = (line + e.toString()) + ", "
        }
    }
    line = line + "]"
    println("Between 7700 and 8000:")
    println(line)
    println("10000th:")
    println(("   [" + (emirps[9999]).toString()) + "]")
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
