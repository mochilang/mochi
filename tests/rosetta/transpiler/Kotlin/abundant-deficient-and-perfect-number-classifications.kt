var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

fun pfacSum(i: Int): Int {
    var sum: Int = 0
    var p: Int = 1
    while (p <= (i / 2)) {
        if ((i % p) == 0) {
            sum = sum + p
        }
        p = p + 1
    }
    return sum
}

fun user_main(): Unit {
    var d: Int = 0
    var a: Int = 0
    var pnum: Int = 0
    var i: Int = 1
    while (i <= 20000) {
        val j: Int = pfacSum(i)
        if (j < i) {
            d = d + 1
        }
        if (j == i) {
            pnum = pnum + 1
        }
        if (j > i) {
            a = a + 1
        }
        i = i + 1
    }
    println(("There are " + d.toString()) + " deficient numbers between 1 and 20000")
    println(("There are " + a.toString()) + " abundant numbers  between 1 and 20000")
    println(("There are " + pnum.toString()) + " perfect numbers between 1 and 20000")
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
