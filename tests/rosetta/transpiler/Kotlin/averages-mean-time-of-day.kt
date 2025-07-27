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

val PI: Double = 3.141592653589793
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 8) {
        val denom: Double = ((2 * n) * ((2 * n) + 1)).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun cosApprox(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = 1
    while (n <= 8) {
        val denom: Double = (((2 * n) - 1) * (2 * n)).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun atanApprox(x: Double): Double {
    if (x > 1.0) {
        return (PI / 2.0) - (x / ((x * x) + 0.28))
    }
    if (x < (0.0 - 1.0)) {
        return ((0.0 - PI) / 2.0) - (x / ((x * x) + 0.28))
    }
    return x / (1.0 + ((0.28 * x) * x))
}

fun atan2Approx(y: Double, x: Double): Double {
    if (x > 0.0) {
        return atanApprox(y / x)
    }
    if (x < 0.0) {
        if (y >= 0.0) {
            return atanApprox(y / x) + PI
        }
        return atanApprox(y / x) - PI
    }
    if (y > 0.0) {
        return PI / 2.0
    }
    if (y < 0.0) {
        return (0.0 - PI) / 2.0
    }
    return 0.0
}

fun digit(ch: String): Int {
    val digits: String = "0123456789"
    var i: Int = 0
    while (i < digits.length) {
        if (digits.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0
}

fun parseTwo(s: String, idx: Int): Int {
    return (digit(s.substring(idx, idx + 1) as String) * 10) + digit(s.substring(idx + 1, idx + 2) as String)
}

fun parseSec(s: String): Double {
    val h: Int = parseTwo(s, 0)
    val m: Int = parseTwo(s, 3)
    val sec: Int = parseTwo(s, 6)
    val tmp: Int = (((h * 60) + m) * 60) + sec
    return tmp.toDouble()
}

fun pad(n: Int): String {
    if (n < 10) {
        return "0" + n.toString()
    }
    return n.toString()
}

fun meanTime(times: MutableList<String>): String {
    var ssum: Double = 0.0
    var csum: Double = 0.0
    var i: Int = 0
    while (i < times.size) {
        val sec: Double = parseSec(times[i])
        val ang: Double = ((sec * 2.0) * PI) / 86400.0
        ssum = ssum + sinApprox(ang)
        csum = csum + cosApprox(ang)
        i = i + 1
    }
    var theta: Double = atan2Approx(ssum, csum)
    var frac: Double = theta / (2.0 * PI)
    while (frac < 0.0) {
        frac = frac + 1.0
    }
    val total: Double = frac * 86400.0
    val si: Int = total.toInt()
    val h: Int = (si / 3600).toInt()
    val m: Int = ((Math.floorMod(si, 3600)) / 60).toInt()
    val s: Int = (Math.floorMod(si, 60)).toInt()
    return (((pad(h) + ":") + pad(m)) + ":") + pad(s)
}

fun user_main(): Unit {
    val inputs: MutableList<String> = mutableListOf("23:00:17", "23:40:20", "00:12:45", "00:17:19")
    println(meanTime(inputs))
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
