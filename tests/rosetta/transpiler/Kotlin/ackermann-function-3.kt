import java.math.BigInteger

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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
        e = (e / 2).toInt()
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
        val mi: Any? = m.toInt()
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
            val r: Any? = pow_big(2.toBigInteger(), n.toInt())
            return ((8 * (r as Int)) - 3).toBigInteger()
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
