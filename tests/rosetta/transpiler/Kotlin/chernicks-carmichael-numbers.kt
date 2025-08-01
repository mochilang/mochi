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

fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return n == 2
    }
    if ((Math.floorMod(n, 3)) == 0) {
        return n == 3
    }
    var d: Int = 5
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 2
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

fun bigTrim(a: MutableList<Int>): MutableList<Int> {
    var a: MutableList<Int> = a
    var n: Int = a.size
    while ((n > 1) && (a[n - 1]!! == 0)) {
        a = a.subList(0, n - 1)
        n = n - 1
    }
    return a
}

fun bigFromInt(x: Int): MutableList<Int> {
    if (x == 0) {
        return mutableListOf(0)
    }
    var digits: MutableList<Int> = mutableListOf<Int>()
    var n: Int = x
    while (n > 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(Math.floorMod(n, 10)); _tmp } as MutableList<Int>
        n = n / 10
    }
    return digits
}

fun bigMulSmall(a: MutableList<Int>, m: Int): MutableList<Int> {
    if (m == 0) {
        return mutableListOf(0)
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var carry: Int = 0
    var i: Int = 0
    while (i < a.size) {
        var prod: BigInteger = ((a[i]!! * m) + carry).toBigInteger()
        res = run { val _tmp = res.toMutableList(); _tmp.add((prod.remainder(10.toBigInteger())).toInt()); _tmp } as MutableList<Int>
        carry = (prod.divide(10.toBigInteger())).toInt()
        i = i + 1
    }
    while (carry > 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(Math.floorMod(carry, 10)); _tmp } as MutableList<Int>
        carry = carry / 10
    }
    return bigTrim(res)
}

fun bigToString(a: MutableList<Int>): String {
    var s: String = ""
    var i: BigInteger = (a.size - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        s = s + (a[(i).toInt()]!!).toString()
        i = i.subtract(1.toBigInteger())
    }
    return s
}

fun pow2(k: Int): Int {
    var r: Int = 1
    var i: Int = 0
    while (i < k) {
        r = r * 2
        i = i + 1
    }
    return r
}

fun ccFactors(n: Int, m: Int): MutableList<Int> {
    var p: BigInteger = ((6 * m) + 1).toBigInteger()
    if (!isPrime(p.toInt())) {
        return mutableListOf<Int>()
    }
    var prod: MutableList<Int> = bigFromInt(p.toInt())
    p = ((12 * m) + 1).toBigInteger()
    if (!isPrime(p.toInt())) {
        return mutableListOf<Int>()
    }
    prod = bigMulSmall(prod, p.toInt())
    var i: Int = 1
    while (i <= (n - 2)) {
        p = (((pow2(i) * 9) * m) + 1).toBigInteger()
        if (!isPrime(p.toInt())) {
            return mutableListOf<Int>()
        }
        prod = bigMulSmall(prod, p.toInt())
        i = i + 1
    }
    return prod
}

fun ccNumbers(start: Int, end: Int): Unit {
    var n: Int = start
    while (n <= end) {
        var m: Int = 1
        if (n > 4) {
            m = pow2(n - 4)
        }
        while (true) {
            var num: MutableList<Int> = ccFactors(n, m)
            if (num.size > 0) {
                println((("a(" + n.toString()) + ") = ") + bigToString(num))
                break
            }
            if (n <= 4) {
                m = m + 1
            } else {
                m = m + pow2(n - 4)
            }
        }
        n = n + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        ccNumbers(3, 9)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
