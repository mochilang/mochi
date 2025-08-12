import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun isqrt(n: Int): Int {
    var r: Int = (0).toInt()
    while (((r + 1) * (r + 1)) <= n) {
        r = r + 1
    }
    return r
}

fun solution(n: Int): Int {
    var sieve: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i <= n) {
        sieve = run { val _tmp = sieve.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    _listSet(sieve, 0, true)
    _listSet(sieve, 1, true)
    var limit: Int = (isqrt(n)).toInt()
    var p: Int = (2).toInt()
    while (p <= limit) {
        if (!((sieve[p]!!) as? Boolean ?: false)) {
            var j: BigInteger = ((p * p).toBigInteger())
            while (j.compareTo((n).toBigInteger()) <= 0) {
                _listSet(sieve, (j).toInt(), true)
                j = j.add((p).toBigInteger())
            }
        }
        p = p + 1
    }
    var sum: Int = (0).toInt()
    var k: Int = (2).toInt()
    while (k < n) {
        if (!((sieve[k]!!) as? Boolean ?: false)) {
            sum = sum + k
        }
        k = k + 1
    }
    return sum
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(solution(20000).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
