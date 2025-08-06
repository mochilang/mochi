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

var SYMBOLS: String = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
fun gcd(a: Int, b: Int): Int {
    var x: Int = a
    var y: Int = b
    while (y != 0) {
        var temp: Int = Math.floorMod(x, y)
        x = y
        y = temp
    }
    return x
}

fun mod_inverse(a: Int, m: Int): Int {
    if (gcd(a, m) != 1) {
        panic(((("mod inverse of " + a.toString()) + " and ") + m.toString()) + " does not exist")
    }
    var u1: Int = 1
    var u2: Int = 0
    var u3: Int = a
    var v1: Int = 0
    var v2: Int = 1
    var v3: Int = m
    while (v3 != 0) {
        var q: Int = u3 / v3
        var t1: Int = u1 - (q * v1)
        var t2: Int = u2 - (q * v2)
        var t3: Int = u3 - (q * v3)
        u1 = v1
        u2 = v2
        u3 = v3
        v1 = t1
        v2 = t2
        v3 = t3
    }
    var res: Int = Math.floorMod(u1, m)
    if (res < 0) {
        return res + m
    }
    return res
}

fun find_symbol(ch: String): Int {
    var i: Int = 0
    while (i < SYMBOLS.length) {
        if (SYMBOLS[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun check_keys(key_a: Int, key_b: Int, mode: String): Unit {
    var m: Int = SYMBOLS.length
    if (mode == "encrypt") {
        if (key_a == 1) {
            panic("The affine cipher becomes weak when key A is set to 1. Choose different key")
        }
        if (key_b == 0) {
            panic("The affine cipher becomes weak when key B is set to 0. Choose different key")
        }
    }
    if ((((key_a < 0) || (key_b < 0) as Boolean)) || (key_b > (m - 1))) {
        panic("Key A must be greater than 0 and key B must be between 0 and " + (m - 1).toString())
    }
    if (gcd(key_a, m) != 1) {
        panic(((("Key A " + key_a.toString()) + " and the symbol set size ") + m.toString()) + " are not relatively prime. Choose a different key.")
    }
}

fun encrypt_message(key: Int, message: String): String {
    var m: Int = SYMBOLS.length
    var key_a: Int = key / m
    var key_b: Int = Math.floorMod(key, m)
    check_keys(key_a, key_b, "encrypt")
    var cipher_text: String = ""
    var i: Int = 0
    while (i < message.length) {
        var ch: String = message[i].toString()
        var index: Int = find_symbol(ch)
        if (index >= 0) {
            cipher_text = cipher_text + SYMBOLS[Math.floorMod(((index * key_a) + key_b), m)].toString()
        } else {
            cipher_text = cipher_text + ch
        }
        i = i + 1
    }
    return cipher_text
}

fun decrypt_message(key: Int, message: String): String {
    var m: Int = SYMBOLS.length
    var key_a: Int = key / m
    var key_b: Int = Math.floorMod(key, m)
    check_keys(key_a, key_b, "decrypt")
    var inv: Int = mod_inverse(key_a, m)
    var plain_text: String = ""
    var i: Int = 0
    while (i < message.length) {
        var ch: String = message[i].toString()
        var index: Int = find_symbol(ch)
        if (index >= 0) {
            var n: BigInteger = (((index - key_b) * inv).toBigInteger())
            var pos = n.remainder((m).toBigInteger())
            var final = if (pos.compareTo((0).toBigInteger()) < 0) pos.add((m).toBigInteger()) else pos
            plain_text = plain_text + SYMBOLS[(final).toInt()].toString()
        } else {
            plain_text = plain_text + ch
        }
        i = i + 1
    }
    return plain_text
}

fun user_main(): Unit {
    var key: Int = 4545
    var msg: String = "The affine cipher is a type of monoalphabetic substitution cipher."
    var enc: String = encrypt_message(key, msg)
    println(enc)
    println(decrypt_message(key, enc))
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
