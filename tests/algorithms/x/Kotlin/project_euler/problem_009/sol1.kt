import java.math.BigInteger

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

fun solution(): Int {
    var a: Int = (0).toInt()
    while (a < 300) {
        var b: BigInteger = ((a + 1).toBigInteger())
        while (b.compareTo((400).toBigInteger()) < 0) {
            var c: BigInteger = b.add((1).toBigInteger())
            while (c.compareTo((500).toBigInteger()) < 0) {
                if (((((a).toBigInteger().add((b))).add((c))).compareTo((1000).toBigInteger()) == 0) && ((((a * a)).toBigInteger().add((b.multiply((b))))).compareTo((c.multiply((c)))) == 0)) {
                    return ((((a).toBigInteger().multiply((b))).multiply((c))).toInt())
                }
                c = c.add((1).toBigInteger())
            }
            b = b.add((1).toBigInteger())
        }
        a = a + 1
    }
    return 0 - 1
}

fun solution_fast(): Int {
    var a: Int = (0).toInt()
    while (a < 300) {
        var b: Int = (0).toInt()
        while (b < 400) {
            var c: Int = ((1000 - a) - b).toInt()
            if ((((a < b) && (b < c) as Boolean)) && (((a * a) + (b * b)) == (c * c))) {
                return (a * b) * c
            }
            b = b + 1
        }
        a = a + 1
    }
    return 0 - 1
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(solution_fast().toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
