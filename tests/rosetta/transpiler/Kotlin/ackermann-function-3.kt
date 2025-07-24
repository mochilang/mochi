import java.math.BigInteger

var err: String = ""
fun pow_big(base: BigInteger, exp: Int): BigInteger {
    var result: BigInteger = java.math.BigInteger.valueOf(1)
    var b: BigInteger = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = result.multiply(b)
        }
        b = b.multiply(b)
        e = e / 2.toInt()
    }
    return result
}

fun bit_len(x: BigInteger): Int {
    var n: BigInteger = x
    var c: Int = 0
    while (n.compareTo(0.toBigInteger()) > 0) {
        n = n.divide(2.toBigInteger())
        c = c + 1
    }
    return c
}

fun ackermann2(m: BigInteger, n: BigInteger): BigInteger {
    if (err != "") {
        return 0.toBigInteger()
    }
    if (m.compareTo(3.toBigInteger()) <= 0) {
        val mi: Int = m.toInt()
        if (mi == 0) {
            return n.add(1.toBigInteger())
        }
        if (mi == 1) {
            return n.add(2.toBigInteger())
        }
        if (mi == 2) {
            return (2.toBigInteger().multiply(n)).add(3.toBigInteger())
        }
        if (mi == 3) {
            val nb: Int = bit_len(n)
            if (nb > 64) {
                err = ("A(m,n) had n of " + nb.toString()) + " bits; too large"
                return 0.toBigInteger()
            }
            val r: BigInteger = pow_big(2.toBigInteger(), n.toInt())
            return (8.toBigInteger().multiply(r)).subtract(3.toBigInteger())
        }
    }
    if (bit_len(n) == 0) {
        return ackermann2(m.subtract(1.toBigInteger()), 1.toBigInteger())
    }
    return ackermann2(m.subtract(1.toBigInteger()), ackermann2(m, n.subtract(1.toBigInteger())))
}

fun show(m: Int, n: Int): Unit {
    err = ""
    val res: BigInteger = ackermann2(m.toBigInteger(), n.toBigInteger())
    if (err != "") {
        println((((("A(" + m.toString()) + ", ") + n.toString()) + ") = Error: ") + err)
        return
    }
    if (bit_len(res) <= 256) {
        println((((("A(" + m.toString()) + ", ") + n.toString()) + ") = ") + res.toString())
    } else {
        val s: String = res.toString()
        val pre: String = s.substring(0, 20)
        val suf: String = s.substring(s.length - 20, s.length)
        println((((((((("A(" + m.toString()) + ", ") + n.toString()) + ") = ") + s.length.toString()) + " digits starting/ending with: ") + pre) + "...") + suf)
    }
}

fun user_main(): Unit {
    show(0, 0)
    show(1, 2)
    show(2, 4)
    show(3, 100)
    show(3, 1000000)
    show(4, 1)
    show(4, 2)
    show(4, 3)
}

fun main() {
    user_main()
}
