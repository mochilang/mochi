import java.math.BigInteger

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

fun pow10(n: Int): Double {
    var p: Double = 1.0
    if (n >= 0) {
        var i: Int = (0).toInt()
        while (i < n) {
            p = p * 10.0
            i = i + 1
        }
    } else {
        var i: Int = (0).toInt()
        while (i > n) {
            p = p / 10.0
            i = i - 1
        }
    }
    return p
}

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    var f: Double = (i.toDouble())
    if (f > x) {
        return ((i - 1).toDouble())
    }
    return f
}

fun format_scientific_3(x: Double): String {
    if (x == 0.0) {
        return "0.000e+00"
    }
    var sign: String = ""
    var num: Double = x
    if (num < 0.0) {
        sign = "-"
        num = 0.0 - num
    }
    var exp: Int = (0).toInt()
    while (num >= 10.0) {
        num = num / 10.0
        exp = exp + 1
    }
    while (num < 1.0) {
        num = num * 10.0
        exp = exp - 1
    }
    var temp = kotlin.math.floor((num * 1000.0) + 0.5)
    var scaled: Int = ((temp as Int)).toInt()
    if (scaled == 10000) {
        scaled = 1000
        exp = exp + 1
    }
    var int_part: Int = (scaled / 1000).toInt()
    var frac_part: BigInteger = ((Math.floorMod(scaled, 1000)).toBigInteger())
    var frac_str: String = frac_part.toString()
    while (frac_str.length < 3) {
        frac_str = "0" + frac_str
    }
    var mantissa: String = (int_part.toString() + ".") + frac_str
    var exp_sign: String = "+"
    var exp_abs: Int = (exp).toInt()
    if (exp < 0) {
        exp_sign = "-"
        exp_abs = 0 - exp
    }
    var exp_str: String = exp_abs.toString()
    if (exp_abs < 10) {
        exp_str = "0" + exp_str
    }
    return (((sign + mantissa) + "e") + exp_sign) + exp_str
}

fun orbital_transfer_work(mass_central: Double, mass_object: Double, r_initial: Double, r_final: Double): String {
    var G: Double = 6.6743 * pow10(0 - 11)
    if ((r_initial <= 0.0) || (r_final <= 0.0)) {
        panic("Orbital radii must be greater than zero.")
    }
    var work: Double = (((G * mass_central) * mass_object) / 2.0) * ((1.0 / r_initial) - (1.0 / r_final))
    return format_scientific_3(work)
}

fun test_orbital_transfer_work(): Unit {
    if (orbital_transfer_work(5.972 * pow10(24), 1000.0, 6.371 * pow10(6), 7.0 * pow10(6)) != "2.811e+09") {
        panic("case1 failed")
    }
    if (orbital_transfer_work(5.972 * pow10(24), 500.0, 7.0 * pow10(6), 6.371 * pow10(6)) != "-1.405e+09") {
        panic("case2 failed")
    }
    if (orbital_transfer_work(1.989 * pow10(30), 1000.0, 1.5 * pow10(11), 2.28 * pow10(11)) != "1.514e+11") {
        panic("case3 failed")
    }
}

fun user_main(): Unit {
    test_orbital_transfer_work()
    println(orbital_transfer_work(5.972 * pow10(24), 1000.0, 6.371 * pow10(6), 7.0 * pow10(6)))
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
