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

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun fmt1(x: Double): String {
    var y: Double = ((((((x * 10.0) + 0.5).toInt())).toDouble())) / 10.0
    var s: String = y.toString()
    var dot: Int = s.indexOf(".")
    if (dot < 0) {
        s = s + ".0"
    }
    return s
}

fun listToString1(xs: MutableList<Double>): String {
    var s: String = "["
    var i: Int = 0
    while (i < xs.size) {
        s = s + fmt1(xs[i]!!)
        if (i < (xs.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    return s + "]"
}

fun deconv(g: MutableList<Double>, f: MutableList<Double>): MutableList<Double> {
    var out: MutableList<Double> = mutableListOf<Double>()
    var i: Int = 0
    while (i <= (g.size - f.size)) {
        var sum: Double = g[i]!!
        var j: Int = 1
        while (j < f.size) {
            if (j <= i) {
                sum = sum - (out[i - j]!! * f[j]!!)
            }
            j = j + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(sum / f[0]!!); _tmp }
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var h: MutableList<Double> = mutableListOf(0.0 - 8.0, 0.0 - 9.0, 0.0 - 3.0, 0.0 - 1.0, 0.0 - 6.0, 7.0)
    var f: MutableList<Double> = mutableListOf(0.0 - 3.0, 0.0 - 6.0, 0.0 - 1.0, 8.0, 0.0 - 6.0, 3.0, 0.0 - 1.0, 0.0 - 9.0, 0.0 - 9.0, 3.0, 0.0 - 2.0, 5.0, 2.0, 0.0 - 2.0, 0.0 - 7.0, 0.0 - 1.0)
    var g: MutableList<Double> = mutableListOf(24.0, 75.0, 71.0, 0.0 - 34.0, 3.0, 22.0, 0.0 - 45.0, 23.0, 245.0, 25.0, 52.0, 25.0, 0.0 - 67.0, 0.0 - 96.0, 96.0, 31.0, 55.0, 36.0, 29.0, 0.0 - 43.0, 0.0 - 7.0)
    println(listToString1(h))
    println(listToString1(deconv(g, f)))
    println(listToString1(f))
    println(listToString1(deconv(g, h)))
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
