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

var given: MutableList<String> = mutableListOf("ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB", "DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA", "DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB")
fun idx(ch: String): Int {
    if (ch == "A") {
        return 0
    }
    if (ch == "B") {
        return 1
    }
    if (ch == "C") {
        return 2
    }
    return 3
}

fun user_main(): Unit {
    var res: String = ""
    var i: Int = 0
    while (i < (given[0]!!).length) {
        var counts: MutableList<Int> = mutableListOf(0, 0, 0, 0)
        for (p in given) {
            var ch: String = p.substring(i, i + 1)
            var j: Int = idx(ch)
            (counts[j]) = counts[j]!! + 1
        }
        var j: Int = 0
        while (j < 4) {
            if ((Math.floorMod(counts[j]!!, 2)) == 1) {
                if (j == 0) {
                    res = res + "A"
                } else {
                    if (j == 1) {
                        res = res + "B"
                    } else {
                        if (j == 2) {
                            res = res + "C"
                        } else {
                            res = res + "D"
                        }
                    }
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    println(res)
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
