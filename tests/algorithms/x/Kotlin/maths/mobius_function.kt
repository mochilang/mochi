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

fun primeFactors(n: Int): MutableList<Int> {
    var n: Int = (n).toInt()
    var i: Int = (2).toInt()
    var factors: MutableList<Int> = mutableListOf<Int>()
    while (((i).toLong() * (i).toLong()) <= n) {
        if ((Math.floorMod(n, i)) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(i); _tmp }
            n = n / i
        } else {
            i = i + 1
        }
    }
    if (n > 1) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(n); _tmp }
    }
    return factors
}

fun isSquareFree(factors: MutableList<Int>): Boolean {
    var seen: MutableMap<Int, Boolean> = mutableMapOf<Int, Boolean>()
    for (f in factors) {
        if (f in seen) {
            return false
        }
        (seen)[f] = true
    }
    return true
}

fun mobius(n: Int): Int {
    var factors: MutableList<Int> = primeFactors(n)
    if ((isSquareFree(factors)) as Boolean) {
        return if ((Math.floorMod(factors.size, 2)) == 0) 1 else 0 - 1.toInt()
    }
    return 0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(mobius(24))
        println(mobius(0 - 1))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
