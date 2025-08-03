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

var testCases: MutableList<MutableList<MutableMap<String, Int>>> = mutableListOf(mutableListOf(mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (2)), mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (3))), mutableListOf(mutableMapOf<String, Int>("a" to (2), "n" to (1), "d" to (3)), mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (7))), mutableListOf(mutableMapOf<String, Int>("a" to (4), "n" to (1), "d" to (5)), mutableMapOf<String, Int>("a" to (0 - 1), "n" to (1), "d" to (239))), mutableListOf(mutableMapOf<String, Int>("a" to (5), "n" to (1), "d" to (7)), mutableMapOf<String, Int>("a" to (2), "n" to (3), "d" to (79))), mutableListOf(mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (2)), mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (5)), mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (8))), mutableListOf(mutableMapOf<String, Int>("a" to (4), "n" to (1), "d" to (5)), mutableMapOf<String, Int>("a" to (0 - 1), "n" to (1), "d" to (70)), mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (99))), mutableListOf(mutableMapOf<String, Int>("a" to (5), "n" to (1), "d" to (7)), mutableMapOf<String, Int>("a" to (4), "n" to (1), "d" to (53)), mutableMapOf<String, Int>("a" to (2), "n" to (1), "d" to (4443))), mutableListOf(mutableMapOf<String, Int>("a" to (6), "n" to (1), "d" to (8)), mutableMapOf<String, Int>("a" to (2), "n" to (1), "d" to (57)), mutableMapOf<String, Int>("a" to (1), "n" to (1), "d" to (239))), mutableListOf(mutableMapOf<String, Int>("a" to (8), "n" to (1), "d" to (10)), mutableMapOf<String, Int>("a" to (0 - 1), "n" to (1), "d" to (239)), mutableMapOf<String, Int>("a" to (0 - 4), "n" to (1), "d" to (515))), mutableListOf(mutableMapOf<String, Int>("a" to (12), "n" to (1), "d" to (18)), mutableMapOf<String, Int>("a" to (8), "n" to (1), "d" to (57)), mutableMapOf<String, Int>("a" to (0 - 5), "n" to (1), "d" to (239))), mutableListOf(mutableMapOf<String, Int>("a" to (16), "n" to (1), "d" to (21)), mutableMapOf<String, Int>("a" to (3), "n" to (1), "d" to (239)), mutableMapOf<String, Int>("a" to (4), "n" to (3), "d" to (1042))), mutableListOf(mutableMapOf<String, Int>("a" to (22), "n" to (1), "d" to (28)), mutableMapOf<String, Int>("a" to (2), "n" to (1), "d" to (443)), mutableMapOf<String, Int>("a" to (0 - 5), "n" to (1), "d" to (1393)), mutableMapOf<String, Int>("a" to (0 - 10), "n" to (1), "d" to (11018))), mutableListOf(mutableMapOf<String, Int>("a" to (22), "n" to (1), "d" to (38)), mutableMapOf<String, Int>("a" to (17), "n" to (7), "d" to (601)), mutableMapOf<String, Int>("a" to (10), "n" to (7), "d" to (8149))), mutableListOf(mutableMapOf<String, Int>("a" to (44), "n" to (1), "d" to (57)), mutableMapOf<String, Int>("a" to (7), "n" to (1), "d" to (239)), mutableMapOf<String, Int>("a" to (0 - 12), "n" to (1), "d" to (682)), mutableMapOf<String, Int>("a" to (24), "n" to (1), "d" to (12943))), mutableListOf(mutableMapOf<String, Int>("a" to (88), "n" to (1), "d" to (172)), mutableMapOf<String, Int>("a" to (51), "n" to (1), "d" to (239)), mutableMapOf<String, Int>("a" to (32), "n" to (1), "d" to (682)), mutableMapOf<String, Int>("a" to (44), "n" to (1), "d" to (5357)), mutableMapOf<String, Int>("a" to (68), "n" to (1), "d" to (12943))), mutableListOf(mutableMapOf<String, Int>("a" to (88), "n" to (1), "d" to (172)), mutableMapOf<String, Int>("a" to (51), "n" to (1), "d" to (239)), mutableMapOf<String, Int>("a" to (32), "n" to (1), "d" to (682)), mutableMapOf<String, Int>("a" to (44), "n" to (1), "d" to (5357)), mutableMapOf<String, Int>("a" to (68), "n" to (1), "d" to (12944))))
fun br(n: Int, d: Int): BigRat {
    return _bigrat(_div(_bigrat(n), _bigrat(_bigrat(d))))
}

fun format(ts: MutableList<MutableMap<String, Int>>): String {
    var s: String = "["
    var i: Int = 0
    while (i < ts.size) {
        var t: MutableMap<String, Int> = ts[i]!!
        s = ((((((s + "{") + ((t)["a"] as Int).toString()) + " ") + ((t)["n"] as Int).toString()) + " ") + ((t)["d"] as Int).toString()) + "}"
        if (i < (ts.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    return s + "]"
}

fun tanEval(coef: Int, f: BigRat): BigRat {
    if (coef == 1) {
        return f
    }
    if (coef < 0) {
        return _bigrat(_sub(_bigrat(0), _bigrat(tanEval(0 - coef, f))))
    }
    var ca: Int = coef / 2
    var cb: BigInteger = (coef - ca).toBigInteger()
    var a: BigRat = tanEval(ca, f)
    var b: BigRat = tanEval((cb.toInt()), f)
    return _bigrat(_div(_bigrat(_add(a, b)), _bigrat(_sub(_bigrat(1), (_bigrat(_mul(a, b)))))))
}

fun tans(m: MutableList<MutableMap<String, Int>>): BigRat {
    if (m.size == 1) {
        var t: MutableMap<String, Int> = m[0]!!
        return tanEval((t)["a"] as Int, br((t)["n"] as Int, (t)["d"] as Int))
    }
    var half: Int = m.size / 2
    var a: BigRat = tans(m.subList(0, half))
    var b: BigRat = tans(m.subList(half, m.size))
    return _bigrat(_div(_bigrat(_add(a, b)), _bigrat(_sub(_bigrat(1), (_bigrat(_mul(a, b)))))))
}

fun main() {
    for (ts in testCases) {
        println((("tan " + format(ts)) + " = ") + tans(ts).toString())
    }
}
