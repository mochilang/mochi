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

fun bernoulli(n: Int): BigRat {
    var a: MutableList<BigRat> = mutableListOf<BigRat>()
    var m: Int = 0
    while (m <= n) {
        a = run { val _tmp = a.toMutableList(); _tmp.add(_bigrat(_div(_bigrat(1), _bigrat(m + 1)))); _tmp } as MutableList<BigRat>
        var j: Int = m
        while (j >= 1) {
            (a[j - 1]) = _bigrat(_mul(_bigrat(j), (_bigrat(_sub(a[j - 1]!!, a[j]!!)))))
            j = j - 1
        }
        m = m + 1
    }
    if (n != 1) {
        return a[0]!!
    }
    return _bigrat(_sub(_bigrat(0), a[0]!!))
}

fun binom(n: Int, k: Int): BigInteger {
    if ((k < 0) || (k > n)) {
        return 0.toBigInteger()
    }
    var kk: Int = k
    if (kk > (n - kk)) {
        kk = n - kk
    }
    var res: BigInteger = java.math.BigInteger.valueOf(1)
    var i: Int = 0
    while (i < kk) {
        res = res.multiply((n - i).toBigInteger())
        i = i + 1
        res = res.divide(i.toBigInteger())
    }
    return res
}

fun faulhaberRow(p: Int): MutableList<BigRat> {
    var coeffs: MutableList<BigRat> = mutableListOf<BigRat>()
    var i: Int = 0
    while (i <= p) {
        coeffs = run { val _tmp = coeffs.toMutableList(); _tmp.add(_bigrat(0)); _tmp } as MutableList<BigRat>
        i = i + 1
    }
    var j: Int = 0
    var sign: Int = 0 - 1
    while (j <= p) {
        sign = 0 - sign
        var c: BigRat = _div(_bigrat(1), _bigrat(p + 1))
        if (sign < 0) {
            c = _bigrat(_sub(_bigrat(0), c))
        }
        c = _bigrat(_mul(c, _bigrat(binom(p + 1, j))))
        c = _bigrat(_mul(c, bernoulli(j)))
        (coeffs[p - j]) = c
        j = j + 1
    }
    return coeffs
}

fun ratStr(r: BigRat): String {
    var s: String = r.toString()
    if ((endsWith(s, "/1")) as Boolean) {
        return s.substring(0, s.length - 2) as String
    }
    return s
}

fun endsWith(s: String, suf: String): Boolean {
    if (s.length < suf.length) {
        return false
    }
    return s.substring(s.length - suf.length, s.length) == suf
}

fun user_main(): Unit {
    var p: Int = 0
    while (p < 10) {
        var row: MutableList<BigRat> = faulhaberRow(p)
        var line: String = ""
        var idx: Int = 0
        while (idx < row.size) {
            line = line + (ratStr(row[idx]!!).padStart(5, " "[0])).toString()
            if (idx < (row.size - 1)) {
                line = line + "  "
            }
            idx = idx + 1
        }
        println(line)
        p = p + 1
    }
    println("")
    var k: Int = 17
    var coeffs: MutableList<BigRat> = faulhaberRow(k)
    var nn: BigRat = _bigrat(1000)
    var np: BigRat = _bigrat(1)
    var sum: BigRat = _bigrat(0)
    var i: Int = 0
    while (i < coeffs.size) {
        np = _bigrat(_mul(np, nn))
        sum = _bigrat(_add(sum, (_bigrat(_mul(coeffs[i]!!, np)))))
        i = i + 1
    }
    println(ratStr(sum))
}

fun main() {
    user_main()
}
