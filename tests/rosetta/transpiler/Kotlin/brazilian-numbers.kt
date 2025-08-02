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

fun sameDigits(n: Int, b: Int): Boolean {
    var n: Int = n
    var f: BigInteger = (Math.floorMod(n, b)).toBigInteger()
    n = ((n / b).toInt())
    while (n > 0) {
        if (((Math.floorMod(n, b))).toBigInteger().compareTo((f)) != 0) {
            return false
        }
        n = ((n / b).toInt())
    }
    return true
}

fun isBrazilian(n: Int): Boolean {
    if (n < 7) {
        return false
    }
    if (((Math.floorMod(n, 2)) == 0) && (n >= 8)) {
        return true
    }
    var b: Int = 2
    while (b < (n - 1)) {
        if (((sameDigits(n, b)) as Boolean)) {
            return true
        }
        b = b + 1
    }
    return false
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

fun user_main(): Unit {
    var kinds: MutableList<String> = mutableListOf(" ", " odd ", " prime ")
    for (kind in kinds) {
        println(("First 20" + kind) + "Brazilian numbers:")
        var c: Int = 0
        var n: Int = 7
        while (true) {
            if (((isBrazilian(n)) as Boolean)) {
                println(n.toString() + " ")
                c = c + 1
                if (c == 20) {
                    println("\n")
                    break
                }
            }
            if (kind == " ") {
                n = n + 1
            } else {
                if (kind == " odd ") {
                    n = n + 2
                } else {
                    while (true) {
                        n = n + 2
                        if (((isPrime(n)) as Boolean)) {
                            break
                        }
                    }
                }
            }
        }
    }
    var n: Int = 7
    var c: Int = 0
    while (c < 100000) {
        if (((isBrazilian(n)) as Boolean)) {
            c = c + 1
        }
        n = n + 1
    }
    println("The 100,000th Brazilian number: " + (n - 1).toString())
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
