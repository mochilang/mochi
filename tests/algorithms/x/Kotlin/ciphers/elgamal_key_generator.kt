import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

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

data class GCD(var g: Int = 0, var x: Int = 0, var y: Int = 0)
data class PublicKey(var key_size: Int = 0, var g: Int = 0, var e2: Int = 0, var p: Int = 0)
data class PrivateKey(var key_size: Int = 0, var d: Int = 0)
data class KeyPair(var public_key: PublicKey = PublicKey(key_size = 0, g = 0, e2 = 0, p = 0), var private_key: PrivateKey = PrivateKey(key_size = 0, d = 0))
var seed: Int = 123456789
fun rand(): Int {
    seed = Math.floorMod(((seed * 1103515245) + 12345), 2147483647)
    return seed
}

fun rand_range(min: Int, max: Int): Int {
    return min + (Math.floorMod(rand(), ((max - min) + 1)))
}

fun mod_pow(base: Int, exponent: Int, modulus: Int): Int {
    var result: Int = 1
    var b: BigInteger = ((Math.floorMod(base, modulus)).toBigInteger())
    var e: Int = exponent
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b))).remainder((modulus).toBigInteger())).toInt())
        }
        e = e / 2
        b = (b.multiply((b))).remainder((modulus).toBigInteger())
    }
    return result
}

fun extended_gcd(a: Int, b: Int): GCD {
    if (b == 0) {
        return GCD(g = a, x = 1, y = 0)
    }
    var res: GCD = extended_gcd(b, Math.floorMod(a, b))
    return GCD(g = res.g, x = res.y, y = res.x - ((a / b) * res.y))
}

fun mod_inverse(a: Int, m: Int): Int {
    var res: GCD = extended_gcd(a, m)
    if (res.g != 1) {
        panic("inverse does not exist")
    }
    var r: Int = Math.floorMod(res.x, m)
    if (r < 0) {
        return r + m
    }
    return r
}

fun is_probable_prime(n: Int, k: Int): Boolean {
    if (n <= 1) {
        return false
    }
    if (n <= 3) {
        return true
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return false
    }
    var r: Int = 0
    var d: BigInteger = ((n - 1).toBigInteger())
    while ((d.remainder((2).toBigInteger())).compareTo((0).toBigInteger()) == 0) {
        d = d.divide((2).toBigInteger())
        r = r + 1
    }
    var i: Int = 0
    while (i < k) {
        var a: Int = rand_range(2, n - 2)
        var x: Int = mod_pow(a, (d.toInt()), n)
        if ((x == 1) || (x == (n - 1))) {
            i = i + 1
            continue
        }
        var j: Int = 1
        var found: Boolean = false
        while (j < r) {
            x = mod_pow(x, 2, n)
            if (x == (n - 1)) {
                found = true
                break
            }
            j = j + 1
        }
        if (!found) {
            return false
        }
        i = i + 1
    }
    return true
}

fun generate_large_prime(bits: Int): Int {
    var min: Int = ((pow2(bits - 1)).toInt())
    var max: Long = pow2(bits) - (1).toLong()
    var p: Int = rand_range(min, (max.toInt()))
    if ((Math.floorMod(p, 2)) == 0) {
        p = p + 1
    }
    while (!is_probable_prime(p, 5)) {
        p = p + 2
        if (p > max) {
            p = min + 1
        }
    }
    return p
}

fun primitive_root(p: Int): Int {
    while (true) {
        var g: Int = rand_range(3, p - 1)
        if (mod_pow(g, 2, p) == 1) {
            continue
        }
        if (mod_pow(g, p, p) == 1) {
            continue
        }
        return g
    }
}

fun generate_key(key_size: Int): KeyPair {
    var p: Int = generate_large_prime(key_size)
    var e1: Int = primitive_root(p)
    var d: Int = rand_range(3, p - 1)
    var e2: Int = mod_inverse(mod_pow(e1, d, p), p)
    var public_key: PublicKey = PublicKey(key_size = key_size, g = e1, e2 = e2, p = p)
    var private_key: PrivateKey = PrivateKey(key_size = key_size, d = d)
    return KeyPair(public_key = public_key, private_key = private_key)
}

fun user_main(): Unit {
    var key_size: Int = 16
    var kp: KeyPair = generate_key(key_size)
    var pub: PublicKey = kp.public_key
    var priv: PrivateKey = kp.private_key
    println(((((((("public key: (" + pub.key_size.toString()) + ", ") + pub.g.toString()) + ", ") + pub.e2.toString()) + ", ") + pub.p.toString()) + ")")
    println(((("private key: (" + priv.key_size.toString()) + ", ") + priv.d.toString()) + ")")
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
