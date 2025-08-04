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

fun parseIntDigits(s: String): Int {
    var n: Int = 0
    var i: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (!(ch in digits)) {
            return 0
        }
        n = (n * 10) + (digits)[ch]!!
        i = i + 1
    }
    return n
}

fun parseDC(s: String): Int {
    var s: String = s
    var neg: Boolean = false
    if ((s.length > 0) && (s.substring(0, 1) == "-")) {
        neg = true
        s = s.substring(1, s.length)
    }
    var dollars: Int = 0
    var cents: Int = 0
    var i: Int = 0
    var seenDot: Boolean = false
    var centDigits: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == ".") {
            seenDot = true
            i = i + 1
            continue
        }
        var d: Int = parseIntDigits(ch)
        if ((seenDot as Boolean)) {
            if (centDigits < 2) {
                cents = (cents * 10) + d
                centDigits = centDigits + 1
            }
        } else {
            dollars = (dollars * 10) + d
        }
        i = i + 1
    }
    if (centDigits == 1) {
        cents = cents * 10
    }
    var _val: BigInteger = ((dollars * 100) + cents).toBigInteger()
    if ((neg as Boolean)) {
        _val = (0).toBigInteger().subtract((_val))
    }
    return (_val.toInt())
}

fun parseRate(s: String): Int {
    var s: String = s
    var neg: Boolean = false
    if ((s.length > 0) && (s.substring(0, 1) == "-")) {
        neg = true
        s = s.substring(1, s.length)
    }
    var whole: Int = 0
    var frac: Int = 0
    var digits: Int = 0
    var seenDot: Boolean = false
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == ".") {
            seenDot = true
            i = i + 1
            continue
        }
        var d: Int = parseIntDigits(ch)
        if ((seenDot as Boolean)) {
            if (digits < 4) {
                frac = (frac * 10) + d
                digits = digits + 1
            }
        } else {
            whole = (whole * 10) + d
        }
        i = i + 1
    }
    while (digits < 4) {
        frac = frac * 10
        digits = digits + 1
    }
    var _val: BigInteger = ((whole * 10000) + frac).toBigInteger()
    if ((neg as Boolean)) {
        _val = (0).toBigInteger().subtract((_val))
    }
    return (_val.toInt())
}

fun dcString(dc: Int): String {
    var d: Int = dc / 100
    var n: Int = dc
    if (n < 0) {
        n = 0 - n
    }
    var c: BigInteger = (Math.floorMod(n, 100)).toBigInteger()
    var cstr: String = c.toString()
    if (cstr.length == 1) {
        cstr = "0" + cstr
    }
    return (d.toString() + ".") + cstr
}

fun extend(dc: Int, n: Int): Int {
    return dc * n
}

fun tax(total: Int, rate: Int): Int {
    return ((((total * rate) + 5000) / 10000).toInt())
}

fun padLeft(s: String, n: Int): String {
    var out: String = s
    while (out.length < n) {
        out = " " + out
    }
    return out
}

fun user_main(): Unit {
    var hp: Int = parseDC("5.50")
    var mp: Int = parseDC("2.86")
    var rate: Int = parseRate("0.0765")
    var totalBeforeTax: BigInteger = (extend(hp, (4000000000000000L.toInt())) + extend(mp, 2)).toBigInteger()
    var t: Int = tax((totalBeforeTax.toInt()), rate)
    var total: BigInteger = totalBeforeTax.add((t).toBigInteger())
    println("Total before tax: " + padLeft(dcString((totalBeforeTax.toInt())), 22))
    println("             Tax: " + padLeft(dcString(t), 22))
    println("           Total: " + padLeft(dcString((total.toInt())), 22))
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
