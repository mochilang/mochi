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

var n: Int = 15
var t: MutableList<Int> = mutableListOf<Int>()
fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (_ in 0 until n + 2) {
            t = run { val _tmp = t.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        }
        (t[1]) = 1
        for (i in 1 until n + 1) {
            var j: Int = i
            while (j > 1) {
                (t[j]) = t[j]!! + t[j - 1]!!
                j = j - 1
            }
            (t[(i + 1).toInt()]) = t[i]!!
            j = i + 1
            while (j > 1) {
                (t[j]) = t[j]!! + t[j - 1]!!
                j = j - 1
            }
            var cat: BigInteger = (t[i + 1]!! - t[i]!!).toBigInteger()
            if (i < 10) {
                println(((" " + i.toString()) + " : ") + cat.toString())
            } else {
                println((i.toString() + " : ") + cat.toString())
            }
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
