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

var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
fun to_lowercase(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        var c: String = s[i].toString()
        var j: Int = 0
        var found: Boolean = false
        while (j < 26) {
            if (c == UPPER[j].toString()) {
                res = res + LOWER[j].toString()
                found = true
                break
            }
            j = j + 1
        }
        if (!found) {
            res = res + c
        }
        i = i + 1
    }
    return res
}

fun char_index(c: String): Int {
    var i: Int = 0
    while (i < 26) {
        if (c == LOWER[i].toString()) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun index_char(i: Int): String {
    return LOWER[i].toString()
}

fun encrypt(plaintext: String, key: String): String {
    var plaintext: String = plaintext
    if (plaintext.length == 0) {
        panic("plaintext is empty")
    }
    if (key.length == 0) {
        panic("key is empty")
    }
    var full_key: String = key + plaintext
    plaintext = to_lowercase(plaintext)
    full_key = to_lowercase(full_key)
    var p_i: Int = 0
    var k_i: Int = 0
    var ciphertext: String = ""
    while (p_i < plaintext.length) {
        var p_char: String = plaintext[p_i].toString()
        var p_idx: Int = char_index(p_char)
        if (p_idx < 0) {
            ciphertext = ciphertext + p_char
            p_i = p_i + 1
        } else {
            var k_char: String = full_key[k_i].toString()
            var k_idx: Int = char_index(k_char)
            if (k_idx < 0) {
                k_i = k_i + 1
            } else {
                var c_idx: Int = Math.floorMod((p_idx + k_idx), 26)
                ciphertext = ciphertext + index_char(c_idx)
                k_i = k_i + 1
                p_i = p_i + 1
            }
        }
    }
    return ciphertext
}

fun decrypt(ciphertext: String, key: String): String {
    if (ciphertext.length == 0) {
        panic("ciphertext is empty")
    }
    if (key.length == 0) {
        panic("key is empty")
    }
    var current_key: String = to_lowercase(key)
    var c_i: Int = 0
    var k_i: Int = 0
    var plaintext: String = ""
    while (c_i < ciphertext.length) {
        var c_char: String = ciphertext[c_i].toString()
        var c_idx: Int = char_index(c_char)
        if (c_idx < 0) {
            plaintext = plaintext + c_char
        } else {
            var k_char: String = current_key[k_i].toString()
            var k_idx: Int = char_index(k_char)
            var p_idx: Int = Math.floorMod(((c_idx - k_idx) + 26), 26)
            var p_char: String = index_char(p_idx)
            plaintext = plaintext + p_char
            current_key = current_key + p_char
            k_i = k_i + 1
        }
        c_i = c_i + 1
    }
    return plaintext
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encrypt("hello world", "coffee"))
        println(decrypt("jsqqs avvwo", "coffee"))
        println(encrypt("coffee is good as python", "TheAlgorithms"))
        println(decrypt("vvjfpk wj ohvp su ddylsv", "TheAlgorithms"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
