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
    var factors: MutableList<Int> = mutableListOf()
    var x: Int = n
    while ((x % 2) == 0) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(2); _tmp } as MutableList<Int>
        x = (x / 2).toInt()
    }
    var p: Int = 3
    while ((p * p) <= x) {
        while ((x % p) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(p); _tmp } as MutableList<Int>
            x = (x / p).toInt()
        }
        p = p + 2
    }
    if (x > 1) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
    }
    return factors
}

fun repeat(ch: String, n: Int): String {
    var s: String = ""
    var i: Int = 0
    while (i < n) {
        s = s + ch
        i = i + 1
    }
    return s
}

fun D(n: Double): Double {
    if (n < 0.0) {
        return 0.0 - D(0.0 - n)
    }
    if (n < 2.0) {
        return 0.0
    }
    var factors: MutableList<Int> = mutableListOf()
    if (n < 10000000000000000000.0) {
        factors = primeFactors(n.toInt())
    } else {
        val g: Int = (n / 100.0).toInt()
        factors = primeFactors(g)
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(2); _tmp } as MutableList<Int>
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(2); _tmp } as MutableList<Int>
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(5); _tmp } as MutableList<Int>
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(5); _tmp } as MutableList<Int>
    }
    val c: Int = factors.size
    if (c == 1) {
        return 1.0
    }
    if (c == 2) {
        return (factors[0] + factors[1]).toDouble()
    }
    val d: Double = n / (factors[0]).toDouble()
    return (D(d) * (factors[0]).toDouble()) + d
}

fun pad(n: Int): String {
    var s: String = n.toString()
    while (s.length < 4) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    var vals: MutableList<Int> = mutableListOf()
    var n: Int = 0 - 99
    while (n < 101) {
        vals = run { val _tmp = vals.toMutableList(); _tmp.add((D(n.toDouble())).toInt()); _tmp } as MutableList<Int>
        n = n + 1
    }
    var i: Int = 0
    while (i < vals.size) {
        var line: String = ""
        var j: Int = 0
        while (j < 10) {
            line = line + pad(vals[i + j])
            if (j < 9) {
                line = line + " "
            }
            j = j + 1
        }
        println(line)
        i = i + 10
    }
    var pow: Double = 1.0
    var m: Int = 1
    while (m < 21) {
        pow = pow * 10.0
        var exp: String = m.toString()
        if (exp.length < 2) {
            exp = exp + " "
        }
        var res: String = m.toString() + repeat("0", m - 1)
        println((("D(10^" + exp) + ") / 7 = ") + res)
        m = m + 1
    }
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
