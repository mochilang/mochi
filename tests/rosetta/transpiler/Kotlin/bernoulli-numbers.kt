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

fun bernoulli(n: Int): BigRat {
    var a: MutableList<BigRat> = mutableListOf<BigRat>()
    var m: Int = 0
    while (m <= n) {
        a = run { val _tmp = a.toMutableList(); _tmp.add(_bigrat(_div(_bigrat(1), _bigrat(m + 1)))); _tmp } as MutableList<BigRat>
        var j: Int = m
        while (j >= 1) {
            a[j - 1] = _bigrat(_mul(_bigrat(j), (_bigrat(_sub(a[j - 1]!!, a[j]!!)))))
            j = j - 1
        }
        m = m + 1
    }
    return a[0]!!
}

fun padStart(s: String, width: Int, pad: String): String {
    var out: String = s
    while (out.length < width) {
        out = pad + out
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (i in 0 until 61) {
            var b: BigRat = bernoulli(i)
            if (_num(b).compareTo((0).toBigInteger()) != 0) {
                var numStr: String = _num(b).toString()
                var denStr: String = _denom(b).toString()
                println((((("B(" + (i.toString().padStart(2, " "[0])).toString()) + ") =") + (numStr.padStart(45, " "[0])).toString()) + "/") + denStr)
            }
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
