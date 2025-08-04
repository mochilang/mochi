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

fun pow_big(base: BigInteger, exp: Int): BigInteger {
    var result: BigInteger = java.math.BigInteger.valueOf(1)
    var b: BigInteger = base
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = result.multiply((b))
        }
        b = b.multiply((b))
        e = ((e / 2).toInt())
    }
    return result
}

fun cullen(n: Int): BigInteger {
    var two_n: BigInteger = pow_big((2.toBigInteger()), n)
    return (two_n.multiply(((n.toBigInteger())))).add(((1.toBigInteger())))
}

fun woodall(n: Int): BigInteger {
    return cullen(n).subtract(((2.toBigInteger())))
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
    var cnums: MutableList<BigInteger> = mutableListOf<BigInteger>()
    var i: Int = 1
    while (i <= 20) {
        cnums = run { val _tmp = cnums.toMutableList(); _tmp.add(cullen(i)); _tmp }
        i = i + 1
    }
    println("First 20 Cullen numbers (n * 2^n + 1):")
    println(show_list(cnums))
    var wnums: MutableList<BigInteger> = mutableListOf<BigInteger>()
    i = 1
    while (i <= 20) {
        wnums = run { val _tmp = wnums.toMutableList(); _tmp.add(woodall(i)); _tmp }
        i = i + 1
    }
    println("\nFirst 20 Woodall numbers (n * 2^n - 1):")
    println(show_list(wnums))
    var cprimes: MutableList<BigInteger> = mutableListOf((1.toBigInteger()), (141.toBigInteger()), (4713.toBigInteger()), (5795.toBigInteger()), (6611.toBigInteger()))
    println("\nFirst 5 Cullen primes (in terms of n):")
    println(show_list(cprimes))
    var wprimes: MutableList<BigInteger> = mutableListOf((2.toBigInteger()), (3.toBigInteger()), (6.toBigInteger()), (30.toBigInteger()), (75.toBigInteger()), (81.toBigInteger()), (115.toBigInteger()), (123.toBigInteger()), (249.toBigInteger()), (362.toBigInteger()), (384.toBigInteger()), (462.toBigInteger()))
    println("\nFirst 12 Woodall primes (in terms of n):")
    println(show_list(wprimes))
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
