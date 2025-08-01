import java.math.BigInteger

fun pow_int(base: Int, exp: Int): Int {
    var result: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = result * b
        }
        b = b * b
        e = (e / 2).toInt()
    }
    return result
}

fun pow_big(base: BigInteger, exp: Int): BigInteger {
    var result: BigInteger = java.math.BigInteger.valueOf(1)
    var b: BigInteger = base
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = result.multiply(b)
        }
        b = b.multiply(b)
        e = (e / 2).toInt()
    }
    return result
}

fun parseBigInt(str: String): BigInteger {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: BigInteger = java.math.BigInteger.valueOf(0)
    while (i < str.length) {
        var ch: String = str.substring(i, i + 1)
        var d: Int = ch.toInt()
        n = (n.multiply(10.toBigInteger())).add(d.toBigInteger())
        i = i + 1
    }
    if (neg as Boolean) {
        n = (0).toBigInteger().subtract(n)
    }
    return n
}

fun fermat(n: Int): BigInteger {
    var p: Int = pow_int(2, n)
    return pow_big(2.toBigInteger(), p).add(1.toBigInteger())
}

fun primeFactorsBig(n: BigInteger): MutableList<BigInteger> {
    var factors: MutableList<BigInteger> = mutableListOf<BigInteger>()
    var m: BigInteger = n
    var d: BigInteger = java.math.BigInteger.valueOf(2)
    while ((m.remainder(d)).compareTo(0.toBigInteger()) == 0) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(d); _tmp } as MutableList<BigInteger>
        m = m.divide(d)
    }
    d = 3.toBigInteger()
    while ((d.multiply(d)).compareTo(m) <= 0) {
        while ((m.remainder(d)).compareTo(0.toBigInteger()) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(d); _tmp } as MutableList<BigInteger>
            m = m.divide(d)
        }
        d = d.add(2.toBigInteger())
    }
    if (m.compareTo(1.toBigInteger()) > 0) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(m); _tmp } as MutableList<BigInteger>
    }
    return factors
}

fun show_list(xs: MutableList<BigInteger>): String {
    var line: String = ""
    var i: Int = 0
    while (i < xs.size) {
        line = line + (xs[i]!!).toString()
        if (i < (xs.size - 1)) {
            line = line + " "
        }
        i = i + 1
    }
    return line
}

fun user_main(): Unit {
    var nums: MutableList<BigInteger> = mutableListOf<BigInteger>()
    for (i in 0 until 8) {
        nums = run { val _tmp = nums.toMutableList(); _tmp.add(fermat(i)); _tmp } as MutableList<BigInteger>
    }
    println("First 8 Fermat numbers:")
    for (n in nums) {
        println(n.toString())
    }
    var extra: MutableMap<Int, MutableList<BigInteger>> = mutableMapOf<Int, MutableList<BigInteger>>(6 to (mutableListOf(274177.toBigInteger(), 67280421310721L.toBigInteger())), 7 to (mutableListOf(parseBigInt("59649589127497217"), parseBigInt("5704689200685129054721")))) as MutableMap<Int, MutableList<BigInteger>>
    println("\nFactors:")
    var i: Int = 0
    while (i < nums.size) {
        var facs: MutableList<BigInteger> = mutableListOf<BigInteger>()
        if (i <= 5) {
            facs = primeFactorsBig(nums[i]!!)
        } else {
            facs = (extra)[i] as MutableList<BigInteger>
        }
        println((("F" + i.toString()) + " = ") + show_list(facs))
        i = i + 1
    }
}

fun main() {
    user_main()
}
