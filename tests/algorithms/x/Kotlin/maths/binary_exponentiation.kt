import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun binary_exp_recursive(base: Double, exponent: Int): Double {
    if (exponent < 0) {
        panic("exponent must be non-negative")
    }
    if (exponent == 0) {
        return 1.0
    }
    if ((Math.floorMod(exponent, 2)) == 1) {
        return binary_exp_recursive(base, exponent - 1) * base
    }
    var half: Double = binary_exp_recursive(base, exponent / 2)
    return half * half
}

fun binary_exp_iterative(base: Double, exponent: Int): Double {
    if (exponent < 0) {
        panic("exponent must be non-negative")
    }
    var result: Double = 1.0
    var b: Double = base
    var e: Int = (exponent).toInt()
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = result * b
        }
        b = b * b
        e = e / 2
    }
    return result
}

fun binary_exp_mod_recursive(base: Int, exponent: Int, modulus: Int): Int {
    if (exponent < 0) {
        panic("exponent must be non-negative")
    }
    if (modulus <= 0) {
        panic("modulus must be positive")
    }
    if (exponent == 0) {
        return Math.floorMod(1, modulus)
    }
    if ((Math.floorMod(exponent, 2)) == 1) {
        return Math.floorMod((binary_exp_mod_recursive(base, exponent - 1, modulus) * (Math.floorMod(base, modulus))), modulus)
    }
    var r: Int = (binary_exp_mod_recursive(base, exponent / 2, modulus)).toInt()
    return Math.floorMod((r * r), modulus)
}

fun binary_exp_mod_iterative(base: Int, exponent: Int, modulus: Int): Int {
    if (exponent < 0) {
        panic("exponent must be non-negative")
    }
    if (modulus <= 0) {
        panic("modulus must be positive")
    }
    var result: Int = (Math.floorMod(1, modulus)).toInt()
    var b: BigInteger = ((Math.floorMod(base, modulus)).toBigInteger())
    var e: Int = (exponent).toInt()
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b))).remainder((modulus).toBigInteger())).toInt())
        }
        b = (b.multiply((b))).remainder((modulus).toBigInteger())
        e = e / 2
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(binary_exp_recursive(3.0, 5))
        println(binary_exp_iterative(1.5, 4))
        println(binary_exp_mod_recursive(3, 4, 5))
        println(binary_exp_mod_iterative(11, 13, 7))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
