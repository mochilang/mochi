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

fun toBase(n: Int, b: Int): String {
    if (n == 0) {
        return "0"
    }
    var s: String = ""
    var x: Int = n
    while (x > 0) {
        s = (Math.floorMod(x, b)).toString() + s
        x = (x / b).toInt()
    }
    return s
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str[0].toString() == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    while (i < str.length) {
        n = ((n * 10) + str.substring(i, i + 1).toInt()) - "0".toInt()
        i = i + 1
    }
    if (neg as Boolean) {
        n = 0 - n
    }
    return n
}

fun parseIntBase(s: String, b: Int): Int {
    var n: Int = 0
    var i: Int = 0
    while (i < s.length) {
        n = (n * b) + parseIntStr(s.substring(i, i + 1))
        i = i + 1
    }
    return n
}

fun reverseStr(s: String): String {
    var out: String = ""
    var i: BigInteger = (s.length - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        out = out + s.substring((i).toInt(), (i.add(1.toBigInteger())).toInt())
        i = i.subtract(1.toBigInteger())
    }
    return out
}

fun isPalindrome(s: String): Boolean {
    return s == reverseStr(s)
}

fun isPalindromeBin(n: Int): Boolean {
    var b: String = toBase(n, 2)
    return isPalindrome(b)
}

fun myMin(a: Int, b: Int): Int {
    if (a < b) {
        return a
    }
    return b
}

fun myMax(a: Int, b: Int): Int {
    if (a > b) {
        return a
    }
    return b
}

fun reverse3(n: Int): Int {
    var x: Int = 0
    var y: Int = n
    while (y != 0) {
        x = (x * 3) + (Math.floorMod(y, 3))
        y = (y / 3).toInt()
    }
    return x
}

fun show(n: Int): Unit {
    println("Decimal : " + n.toString())
    println("Binary  : " + toBase(n, 2))
    println("Ternary : " + toBase(n, 3))
    println("")
}

fun user_main(): Unit {
    println("The first 6 numbers which are palindromic in both binary and ternary are :\n")
    show(0)
    var count: Int = 1
    var lo: Int = 0
    var hi: Int = 1
    var pow2: Int = 1
    var pow3: Int = 1
    while (true) {
        var i: Int = lo
        while (i < hi) {
            var n: BigInteger = ((((i * 3) + 1) * pow3) + reverse3(i)).toBigInteger()
            if ((isPalindromeBin(n.toInt())) as Boolean) {
                show(n.toInt())
                count = count + 1
                if (count >= 6) {
                    return
                }
            }
            i = i + 1
        }
        if (i == pow3) {
            pow3 = pow3 * 3
        } else {
            pow2 = pow2 * 4
        }
        while (true) {
            while (pow2 <= pow3) {
                pow2 = pow2 * 4
            }
            var lo2: Int = (((pow2 / pow3) - 1) / 3).toInt()
            var hi2: BigInteger = (((((pow2 * 2) / pow3) - 1) / 3).toInt() + 1).toBigInteger()
            var lo3: Int = (pow3 / 3).toInt()
            var hi3: Int = pow3
            if (lo2 >= hi3) {
                pow3 = pow3 * 3
            } else {
                if ((lo3).toBigInteger().compareTo(hi2) >= 0) {
                    pow2 = pow2 * 4
                } else {
                    lo = myMax(lo2, lo3)
                    hi = myMin(hi2.toInt(), hi3)
                    break
                }
            }
        }
    }
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
