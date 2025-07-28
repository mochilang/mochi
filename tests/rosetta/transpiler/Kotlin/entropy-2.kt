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

fun log2(x: Double): Double {
    var k: Double = 0.0
    var v: Double = x
    while (v >= 2.0) {
        v = v / 2.0
        k = k + 1.0
    }
    while (v < 1.0) {
        v = v * 2.0
        k = k - 1.0
    }
    val z: Double = (v - 1.0) / (v + 1.0)
    var zpow: Double = z
    var sum: Double = z
    var i: Int = 3
    while (i <= 9) {
        zpow = (zpow * z) * z
        sum = sum + (zpow / i.toDouble())
        i = i + 2
    }
    val ln2: Double = 0.6931471805599453
    return k + ((2.0 * sum) / ln2)
}

fun user_main(): Unit {
    val s: String = "1223334444"
    var counts: MutableMap<String, Int> = mutableMapOf<Any?, Any?>() as MutableMap<String, Int>
    var l: Double = 0.0
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if (ch in counts) {
            (counts)[ch] as Int = (counts)[ch] as Int + 1
        } else {
            (counts)[ch] as Int = 1
        }
        l = l + 1.0
        i = i + 1
    }
    var hm: Double = 0.0
    for (ch in counts.keys) {
        val c: Double = ((counts)[ch] as Int).toDouble()
        hm = hm + (c * log2(c))
    }
    println((log2(l) - (hm / l)).toString())
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
