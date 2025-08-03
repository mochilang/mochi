import java.math.BigInteger

data class BigRat(var num: BigInteger, var den: BigInteger = BigInteger.ONE) {
    init {
        if (den.signum() < 0) {
            num = num.negate()
            den = den.negate()
        }
        val g = num.gcd(den)
        num = num.divide(g)
        den = den.divide(g)
    }
    fun add(o: BigRat) = BigRat(num.multiply(o.den).add(o.num.multiply(den)), den.multiply(o.den))
    fun sub(o: BigRat) = BigRat(num.multiply(o.den).subtract(o.num.multiply(den)), den.multiply(o.den))
    fun mul(o: BigRat) = BigRat(num.multiply(o.num), den.multiply(o.den))
    fun div(o: BigRat) = BigRat(num.multiply(o.den), den.multiply(o.num))
}
fun _bigrat(n: Any?, d: Any? = 1): BigRat {
    if (n is BigRat && d == null) return BigRat(n.num, n.den)
    val denom = when (d) {
        null -> BigInteger.ONE
        is BigInteger -> d
        is Number -> BigInteger.valueOf(d.toLong())
        else -> BigInteger.ONE
    }
    val numer = when (n) {
        is BigRat -> n.num
        is BigInteger -> n
        is Number -> BigInteger.valueOf(n.toLong())
        else -> BigInteger.ZERO
    }
    val den = if (n is BigRat && d == null) n.den else denom
    return BigRat(numer, den)
}
fun _num(r: BigRat): BigInteger = r.num
fun _denom(r: BigRat): BigInteger = r.den
fun _add(a: BigRat, b: BigRat): BigRat = a.add(b)
fun _sub(a: BigRat, b: BigRat): BigRat = a.sub(b)
fun _mul(a: BigRat, b: BigRat): BigRat = a.mul(b)
fun _div(a: BigRat, b: BigRat): BigRat = a.div(b)

fun repeat(s: String, n: Int): String {
    val sb = StringBuilder()
    repeat(n) { sb.append(s) }
    return sb.toString()
}

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

fun bigrat(a: Int, b: Int): BigRat {
    return _bigrat(_div(_bigrat(a), _bigrat(b)))
}

fun calkinWilf(n: Int): MutableList<BigRat> {
    var seq: MutableList<BigRat> = mutableListOf<BigRat>()
    seq = run { val _tmp = seq.toMutableList(); _tmp.add(_bigrat(1, 1)); _tmp }
    var i: Int = 1
    while (i < n) {
        var prev: BigRat = seq[i - 1]!!
        var a: BigInteger = _num(prev)
        var b: BigInteger = _denom(prev)
        var f: BigInteger = a.divide((b))
        var t: BigRat = _bigrat(f, 1)
        t = _bigrat(_mul(t, _bigrat(2)))
        t = _bigrat(_sub(t, prev))
        t = _bigrat(_add(t, _bigrat(1)))
        t = _bigrat(_div(_bigrat(1), t))
        seq = run { val _tmp = seq.toMutableList(); _tmp.add(t); _tmp }
        i = i + 1
    }
    return seq
}

fun toContinued(r: BigRat): MutableList<Int> {
    var a: BigInteger = _num(r)
    var b: BigInteger = _denom(r)
    var res: MutableList<Int> = mutableListOf<Int>()
    while (true) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(((a.divide((b))).toInt())); _tmp }
        var t: BigInteger = a.remainder((b))
        a = b
        b = t
        if (a.compareTo((1).toBigInteger()) == 0) {
            break
        }
    }
    if ((Math.floorMod(res.size, 2)) == 0) {
        res[res.size - 1] = res[res.size - 1]!! - 1
        res = run { val _tmp = res.toMutableList(); _tmp.add(1); _tmp }
    }
    return res
}

fun termNumber(cf: MutableList<Int>): Int {
    var b: String = ""
    var d: String = "1"
    for (n in cf) {
        b = (repeat(d, n)).toString() + b
        if (d == "1") {
            d = "0"
        } else {
            d = "1"
        }
    }
    return ((Integer.parseInt(b, 2)) as Int)
}

fun commatize(n: Int): String {
    var s: String = n.toString()
    var out: String = ""
    var i: Int = 0
    var cnt: Int = 0
    var neg: Boolean = false
    if (s.substring(0, 1) == "-") {
        neg = true
        s = s.substring(1, s.length)
    }
    i = s.length - 1
    while (i >= 0) {
        out = s.substring(i, i + 1) + out
        cnt = cnt + 1
        if ((cnt == 3) && (i != 0)) {
            out = "," + out
            cnt = 0
        }
        i = i - 1
    }
    if ((neg as Boolean)) {
        out = "-" + out
    }
    return out
}

fun user_main(): Unit {
    var cw: MutableList<BigRat> = calkinWilf(20)
    println("The first 20 terms of the Calkin-Wilf sequnence are:")
    var i: Int = 0
    while (i < 20) {
        var r: BigRat = cw[i]!!
        var s: String = _num(r).toString()
        if (_denom(r).compareTo((1).toBigInteger()) != 0) {
            s = (s + "/") + _denom(r).toString()
        }
        println((((i + ((1.toInt()))).toString().padStart(2, ' ')).toString() + ": ") + s)
        i = i + 1
    }
    var r: BigRat = _bigrat(83116, 51639)
    var cf: MutableList<Int> = toContinued(r)
    var tn: Int = termNumber(cf)
    println(((((("" + _num(r).toString()) + "/") + _denom(r).toString()) + " is the ") + commatize(tn)) + "th term of the sequence.")
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
