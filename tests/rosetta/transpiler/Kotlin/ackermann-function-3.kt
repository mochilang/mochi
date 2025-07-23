var err: String = ""
fun pow_big(base: bigint, exp: Int): bigint {
    var result: bigint = 1
    var b: bigint = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = (result as Number).toDouble() * (b as Number).toDouble()
        }
        b = (b as Number).toDouble() * (b as Number).toDouble()
        e = e / 2.toInt() as Int
    }
    return result as bigint
}

fun bit_len(x: bigint): Int {
    var n: bigint = x
    var c: Int = 0
    while ((n as Number).toDouble() > 0) {
        n = (n as Int) / 2
        c = c + 1
    }
    return c
}

fun ackermann2(m: bigint, n: bigint): bigint {
    if (err != "") {
        return 0 as bigint
    }
    if ((m as Number).toDouble() <= 3) {
        val mi: int = m.toInt()
        if (mi == 0) {
            return (n as Int) + 1 as bigint
        }
        if (mi == 1) {
            return (n as Int) + 2 as bigint
        }
        if (mi == 2) {
            return (2 * (n as Int)) + 3 as bigint
        }
        if (mi == 3) {
            val nb: Int = bit_len(n) as Int
            if (nb > 64) {
                err = ("A(m,n) had n of " + nb.toString()) + " bits; too large"
                return 0 as bigint
            }
            val r = pow_big(2 as bigint, n.toInt() as Int)
            return (8 * (r as Int)) - 3 as bigint
        }
    }
    if (bit_len(n) as Int == 0) {
        return ackermann2((m as Number).toDouble() - 1 as bigint, 1 as bigint) as bigint
    }
    return ackermann2((m as Number).toDouble() - 1 as bigint, ackermann2(m, (n as Number).toDouble() - 1 as bigint)) as bigint
}

fun show(m: Int, n: Int): Unit {
    err = ""
    val res = ackermann2(m as bigint, n as bigint)
    if (err != "") {
        println((((("A(" + m.toString()) + ", ") + n.toString()) + ") = Error: ") + err)
        return
    }
    if (bit_len(res) as Int <= 256) {
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
