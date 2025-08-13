fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var BOLTZMANN: Double = 1.380649 / pow10(23)
var ELECTRON_VOLT: Double = 1.602176634 / pow10(19)
var TEMPERATURE: Double = 300.0
fun pow10(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun ln_series(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var n: Int = (1).toInt()
    while (n <= 19) {
        sum = sum + (term / ((n.toDouble())))
        term = (term * t) * t
        n = n + 2
    }
    return 2.0 * sum
}

fun ln(x: Double): Double {
    var y: Double = x
    var k: Int = (0).toInt()
    while (y >= 10.0) {
        y = y / 10.0
        k = k + 1
    }
    while (y < 1.0) {
        y = y * 10.0
        k = k - 1
    }
    return ln_series(y) + (((k.toDouble())) * ln_series(10.0))
}

fun builtin_voltage(donor_conc: Double, acceptor_conc: Double, intrinsic_conc: Double): Double {
    if (donor_conc <= 0.0) {
        panic("Donor concentration should be positive")
    }
    if (acceptor_conc <= 0.0) {
        panic("Acceptor concentration should be positive")
    }
    if (intrinsic_conc <= 0.0) {
        panic("Intrinsic concentration should be positive")
    }
    if (donor_conc <= intrinsic_conc) {
        panic("Donor concentration should be greater than intrinsic concentration")
    }
    if (acceptor_conc <= intrinsic_conc) {
        panic("Acceptor concentration should be greater than intrinsic concentration")
    }
    return ((BOLTZMANN * TEMPERATURE) * ln((donor_conc * acceptor_conc) / (intrinsic_conc * intrinsic_conc))) / ELECTRON_VOLT
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(builtin_voltage(pow10(17), pow10(17), pow10(10)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
