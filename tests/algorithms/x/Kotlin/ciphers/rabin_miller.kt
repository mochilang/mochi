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

var p: Int = generate_large_prime(16)
fun int_pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun pow_mod(base: Int, exp: Int, mod: Int): Int {
    var result: Int = 1
    var b: BigInteger = ((Math.floorMod(base, mod)).toBigInteger())
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b).toBigInteger())).remainder((mod).toBigInteger())).toInt())
        }
        e = e / 2
        b = (b.multiply((b).toBigInteger())).remainder((mod).toBigInteger())
    }
    return result
}

fun rand_range(low: Int, high: Int): Int {
    return (Math.floorMod(_now(), (high - low))) + low
}

fun rabin_miller(num: Int): Boolean {
    var s: Int = num - 1
    var t: Int = 0
    while ((Math.floorMod(s, 2)) == 0) {
        s = s / 2
        t = t + 1
    }
    var k: Int = 0
    while (k < 5) {
        var a: Int = rand_range(2, num - 1)
        var v: Int = pow_mod(a, s, num)
        if (v != 1) {
            var i: Int = 0
            while (v != (num - 1)) {
                if (i == (t - 1)) {
                    return false
                }
                i = i + 1
                v = Math.floorMod((v * v), num)
            }
        }
        k = k + 1
    }
    return true
}

fun is_prime_low_num(num: Int): Boolean {
    if (num < 2) {
        return false
    }
    var low_primes: MutableList<Int> = mutableListOf(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997)
    if (num in low_primes) {
        return true
    }
    var i: Int = 0
    while (i < low_primes.size) {
        var p: Int = low_primes[i]!!
        if ((Math.floorMod(num, p)) == 0) {
            return false
        }
        i = i + 1
    }
    return rabin_miller(num)
}

fun generate_large_prime(keysize: Int): Int {
    var start: Int = int_pow(2, keysize - 1)
    var end: Int = int_pow(2, keysize)
    while (true) {
        var num: Int = rand_range(start, end)
        if (((is_prime_low_num(num)) as Boolean)) {
            return num
        }
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Prime number: " + p.toString())
        println("is_prime_low_num: " + is_prime_low_num(p).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
