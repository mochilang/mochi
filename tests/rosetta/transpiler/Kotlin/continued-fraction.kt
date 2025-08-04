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

fun newTerm(a: Int, b: Int): MutableMap<String, Int> {
    return mutableMapOf<String, Int>("a" to (a), "b" to (b))
}

fun cfSqrt2(nTerms: Int): MutableList<MutableMap<String, Int>> {
    var f: MutableList<MutableMap<String, Int>> = mutableListOf<MutableMap<String, Int>>()
    var n: Int = 0
    while (n < nTerms) {
        f = run { val _tmp = f.toMutableList(); _tmp.add(newTerm(2, 1)); _tmp }
        n = n + 1
    }
    if (nTerms > 0) {
        (f[0]!!)["a"] = 1
    }
    return f
}

fun cfNap(nTerms: Int): MutableList<MutableMap<String, Int>> {
    var f: MutableList<MutableMap<String, Int>> = mutableListOf<MutableMap<String, Int>>()
    var n: Int = 0
    while (n < nTerms) {
        f = run { val _tmp = f.toMutableList(); _tmp.add(newTerm(n, n - 1)); _tmp }
        n = n + 1
    }
    if (nTerms > 0) {
        (f[0]!!)["a"] = 2
    }
    if (nTerms > 1) {
        (f[1]!!)["b"] = 1
    }
    return f
}

fun cfPi(nTerms: Int): MutableList<MutableMap<String, Int>> {
    var f: MutableList<MutableMap<String, Int>> = mutableListOf<MutableMap<String, Int>>()
    var n: Int = 0
    while (n < nTerms) {
        var g: BigInteger = ((2 * n) - 1).toBigInteger()
        f = run { val _tmp = f.toMutableList(); _tmp.add(newTerm(6, ((g.multiply((g))).toInt()))); _tmp }
        n = n + 1
    }
    if (nTerms > 0) {
        (f[0]!!)["a"] = 3
    }
    return f
}

fun real(f: MutableList<MutableMap<String, Int>>): Double {
    var r: Double = 0.0
    var i: BigInteger = (f.size - 1).toBigInteger()
    while (i.compareTo((0).toBigInteger()) > 0) {
        r = ((((((f[(i).toInt()]!!) as MutableMap<String, Int>))["b"]!!).toDouble())) / (((((((f[(i).toInt()]!!) as MutableMap<String, Int>))["a"]!!).toDouble())) + r)
        i = i.subtract((1).toBigInteger())
    }
    if (f.size > 0) {
        r = r + ((((((f[0]!!) as MutableMap<String, Int>))["a"]!!).toDouble()))
    }
    return r
}

fun user_main(): Unit {
    println("sqrt2: " + real(cfSqrt2(20)).toString())
    println("nap:   " + real(cfNap(20)).toString())
    println("pi:    " + real(cfPi(20)).toString())
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
