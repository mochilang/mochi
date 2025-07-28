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

fun fmtF3(x: Double): String {
    var y: Double = floorf((x * 1000.0) + 0.5) / 1000.0
    var s: String = y.toString()
    var dot: Int = indexOf(s, ".")
    if (dot == (0 - 1)) {
        s = s + ".000"
    } else {
        var decs: BigInteger = ((s.length - dot) - 1).toBigInteger()
        if (decs.compareTo(3.toBigInteger()) > 0) {
            s = s.substring(0, dot + 4) as String
        } else {
            while (decs.compareTo(3.toBigInteger()) < 0) {
                s = s + "0"
                decs = decs.add(1.toBigInteger())
            }
        }
    }
    return s
}

fun padFloat3(x: Double, width: Int): String {
    var s: String = fmtF3(x)
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun fib1000(): MutableList<Double> {
    var a: Double = 0.0
    var b: Double = 1.0
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = 0
    while (i < 1000) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(b); _tmp } as MutableList<Double>
        var t: Double = b
        b = b + a
        a = t
        i = i + 1
    }
    return res
}

fun leadingDigit(x: Double): Int {
    var x: Double = x
    if (x < 0.0) {
        x = 0.0 - x
    }
    while (x >= 10.0) {
        x = x / 10.0
    }
    while ((x > 0.0) && (x < 1.0)) {
        x = x * 10.0
    }
    return x.toInt()
}

fun show(nums: MutableList<Double>, title: String): Unit {
    var counts: MutableList<Int> = mutableListOf(0, 0, 0, 0, 0, 0, 0, 0, 0)
    for (n in nums) {
        val d: Int = leadingDigit(n)
        if ((d >= 1) && (d <= 9)) {
            counts[d - 1] = counts[d - 1] + 1
        }
    }
    val preds: MutableList<Double> = mutableListOf(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
    val total: Int = nums.size
    println(title)
    println("Digit  Observed  Predicted")
    var i: Int = 0
    while (i < 9) {
        val obs: Double = (counts[i]).toDouble() / total.toDouble()
        var line: String = (((("  " + (i + 1).toString()) + "  ") + padFloat3(obs, 9)) + "  ") + padFloat3(preds[i], 8)
        println(line)
        i = i + 1
    }
}

fun user_main(): Unit {
    show(fib1000(), "First 1000 Fibonacci numbers")
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
