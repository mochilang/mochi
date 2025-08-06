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

var ALPHABET: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var message: String = "THE GERMAN ATTACK"
var key: String = "SECRET"
var key_new: String = generate_key(message, key)
var encrypted: String = cipher_text(message, key_new)
fun index_of(ch: String): Int {
    for (i in 0 until ALPHABET.length) {
        if (ALPHABET[i].toString() == ch) {
            return i
        }
    }
    return 0 - 1
}

fun generate_key(message: String, key: String): String {
    var key_new: String = key
    var i: Int = 0
    while (key_new.length < message.length) {
        key_new = key_new + key[i].toString()
        i = i + 1
        if (i == key.length) {
            i = 0
        }
    }
    return key_new
}

fun cipher_text(message: String, key_new: String): String {
    var res: String = ""
    var i: Int = 0
    for (idx in 0 until message.length) {
        var ch: String = message[idx].toString()
        if (ch == " ") {
            res = res + " "
        } else {
            var x: Int = Math.floorMod(((index_of(ch) - index_of(key_new[i].toString())) + 26), 26)
            i = i + 1
            res = res + ALPHABET[x].toString()
        }
    }
    return res
}

fun original_text(cipher: String, key_new: String): String {
    var res: String = ""
    var i: Int = 0
    for (idx in 0 until cipher.length) {
        var ch: String = cipher[idx].toString()
        if (ch == " ") {
            res = res + " "
        } else {
            var x: Int = Math.floorMod(((index_of(ch) + index_of(key_new[i].toString())) + 26), 26)
            i = i + 1
            res = res + ALPHABET[x].toString()
        }
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Encrypted Text = " + encrypted)
        println("Original Text = " + original_text(encrypted, key_new))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
