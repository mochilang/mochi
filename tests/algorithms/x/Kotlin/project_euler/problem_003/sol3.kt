fun json(v: Any?) { println(toJson(v)) }

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

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

fun largest_prime_factor(n: Int): Int {
    if (n <= 1) {
        return n
    }
    var i: Int = (2).toInt()
    var ans: Int = (0).toInt()
    var m: Int = (n).toInt()
    if (m == 2) {
        return 2
    }
    while (m > 2) {
        while ((Math.floorMod(m, i)) != 0) {
            i = i + 1
        }
        ans = i
        while ((Math.floorMod(m, i)) == 0) {
            m = m / i
        }
        i = i + 1
    }
    return ans
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        json(((largest_prime_factor(13195)) as Any?))
        json(((largest_prime_factor(10)) as Any?))
        json(((largest_prime_factor(17)) as Any?))
        json(((largest_prime_factor((600851475143L.toInt()))) as Any?))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
