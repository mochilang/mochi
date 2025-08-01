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

fun fmt3(x: Double): String {
    var y: Double = ((((x * 1000.0) + 0.5).toInt()).toDouble()) / 1000.0
    var s: String = y.toString()
    var dot: Int = s.indexOf(".")
    if (dot == (0 - 1)) {
        s = s + ".000"
    } else {
        var decs: BigInteger = ((s.length - dot) - 1).toBigInteger()
        if (decs.compareTo((3).toBigInteger()) > 0) {
            s = s.substring(0, dot + 4)
        } else {
            while (decs.compareTo((3).toBigInteger()) < 0) {
                s = s + "0"
                decs = decs.add((1).toBigInteger())
            }
        }
    }
    return s
}

fun pad(s: String, width: Int): String {
    var out: String = s
    while (out.length < width) {
        out = " " + out
    }
    return out
}

fun smaSeries(xs: MutableList<Double>, period: Int): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var sum: Double = 0.0
    var i: Int = 0
    while (i < xs.size) {
        sum = sum + xs[i]!!
        if (i >= period) {
            sum = sum - xs[i - period]!!
        }
        var denom: BigInteger = (i + 1).toBigInteger()
        if (denom.compareTo((period).toBigInteger()) > 0) {
            denom = period.toBigInteger()
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(sum / (denom.toDouble())); _tmp } as MutableList<Double>
        i = i + 1
    }
    return res
}

fun user_main(): Unit {
    var xs: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0)
    var sma3: MutableList<Double> = smaSeries(xs, 3)
    var sma5: MutableList<Double> = smaSeries(xs, 5)
    println("x       sma3   sma5")
    var i: Int = 0
    while (i < xs.size) {
        var line: String = (((pad(fmt3(xs[i]!!), 5) + "  ") + pad(fmt3(sma3[i]!!), 5)) + "  ") + pad(fmt3(sma5[i]!!), 5)
        println(line)
        i = i + 1
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
