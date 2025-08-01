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

fun floorf(x: Double): Double {
    var y: Int = x.toInt()
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

fun fmt8(x: Double): String {
    var y: Double = floorf((x * 100000000.0) + 0.5) / 100000000.0
    var s: String = y.toString()
    var dot: Int = s.indexOf(".")
    if (dot == (0 - 1)) {
        s = s + ".00000000"
    } else {
        var decs: BigInteger = ((s.length - dot) - 1).toBigInteger()
        while (decs.compareTo(8.toBigInteger()) < 0) {
            s = s + "0"
            decs = decs.add(1.toBigInteger())
        }
    }
    return s
}

fun pad2(x: Int): String {
    var s: String = x.toString()
    if (s.length < 2) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    var maxIt: Int = 13
    var maxItJ: Int = 10
    var a1: Double = 1.0
    var a2: Double = 0.0
    var d1: Double = 3.2
    println(" i       d")
    var i: Int = 2
    while (i <= maxIt) {
        var a: Double = a1 + ((a1 - a2) / d1)
        var j: Int = 1
        while (j <= maxItJ) {
            var x: Double = 0.0
            var y: Double = 0.0
            var k: Int = 1
            var limit: Int = pow_int(2, i)
            while (k <= limit) {
                y = 1.0 - ((2.0 * y) * x)
                x = a - (x * x)
                k = k + 1
            }
            a = a - (x / y)
            j = j + 1
        }
        var d: Double = (a1 - a2) / (a - a1)
        println((pad2(i) + "    ") + fmt8(d))
        d1 = d
        a2 = a1
        a1 = a
        i = i + 1
    }
}

fun pow_int(base: Int, exp: Int): Int {
    var r: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            r = r * b
        }
        b = b * b
        e = (e / 2).toInt()
    }
    return r
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
