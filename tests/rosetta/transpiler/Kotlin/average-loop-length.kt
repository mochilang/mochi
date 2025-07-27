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

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun floorf(x: Double): Double {
    val y: Int = x.toInt()
    return y.toDouble()
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

fun fmtF(x: Double): String {
    var y: Double = floorf((x * 10000.0) + 0.5) / 10000.0
    var s: String = y.toString()
    var dot: Int = indexOf(s, ".")
    if (dot == (0 - 1)) {
        s = s + ".0000"
    } else {
        var decs: BigInteger = ((s.length - dot) - 1).toBigInteger()
        if (decs.compareTo(4.toBigInteger()) > 0) {
            s = s.substring(0, dot + 5) as String
        } else {
            while (decs.compareTo(4.toBigInteger()) < 0) {
                s = s + "0"
                decs = decs.add(1.toBigInteger())
            }
        }
    }
    return s
}

fun padInt(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun padFloat(x: Double, width: Int): String {
    var s: String = fmtF(x)
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun avgLen(n: Int): Double {
    val tests: Int = 10000
    var sum: Int = 0
    var seed: Int = 1
    var t: Int = 0
    while (t < tests) {
        var visited: MutableList<Boolean> = mutableListOf()
        var i: Int = 0
        while (i < n) {
            visited = run { val _tmp = visited.toMutableList(); _tmp.add(false); _tmp } as MutableList<Boolean>
            i = i + 1
        }
        var x: Int = 0
        while (!visited[x]) {
            visited[x] = true
            sum = sum + 1
            seed = Math.floorMod(((seed * 1664525) + 1013904223), 2147483647)
            x = Math.floorMod(seed, n)
        }
        t = t + 1
    }
    return sum.toDouble() / tests
}

fun ana(n: Int): Double {
    var nn: Double = n.toDouble()
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Double = nn - 1.0
    while (i >= 1.0) {
        term = term * (i / nn)
        sum = sum + term
        i = i - 1.0
    }
    return sum
}

fun user_main(): Unit {
    val nmax: Int = 20
    println(" N    average    analytical    (error)")
    println("===  =========  ============  =========")
    var n: Int = 1
    while (n <= nmax) {
        val a: Double = avgLen(n)
        val b: Double = ana(n)
        val err: Double = (absf(a - b) / b) * 100.0
        var line: String = ((((((padInt(n, 3) + "  ") + padFloat(a, 9)) + "  ") + padFloat(b, 12)) + "  (") + padFloat(err, 6)) + "%)"
        println(line)
        n = n + 1
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
