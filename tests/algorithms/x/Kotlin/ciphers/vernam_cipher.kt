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

var plaintext: String = "HELLO"
var key: String = "KEY"
var encrypted: String = vernam_encrypt(plaintext, key)
var decrypted: String = vernam_decrypt(encrypted, key)
fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var idx: Int = upper.indexOf(ch)
    if (idx >= 0) {
        return 65 + idx
    }
    return 0
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    if ((n >= 65) && (n < 91)) {
        return upper.substring(n - 65, n - 64)
    }
    return "?"
}

fun vernam_encrypt(plaintext: String, key: String): String {
    var ciphertext: String = ""
    var i: Int = 0
    while (i < plaintext.length) {
        var p: Int = ord(plaintext.substring(i, i + 1)) - 65
        var k: Int = ord(key.substring(Math.floorMod(i, key.length), (Math.floorMod(i, key.length)) + 1)) - 65
        var ct: BigInteger = ((p + k).toBigInteger())
        while (ct.compareTo((25).toBigInteger()) > 0) {
            ct = ct.subtract((26).toBigInteger())
        }
        ciphertext = ciphertext + chr(((ct.add((65).toBigInteger())).toInt()))
        i = i + 1
    }
    return ciphertext
}

fun vernam_decrypt(ciphertext: String, key: String): String {
    var decrypted: String = ""
    var i: Int = 0
    while (i < ciphertext.length) {
        var c: Int = ord(ciphertext.substring(i, i + 1))
        var k: Int = ord(key.substring(Math.floorMod(i, key.length), (Math.floorMod(i, key.length)) + 1))
        var _val: BigInteger = ((c - k).toBigInteger())
        while (_val.compareTo((0).toBigInteger()) < 0) {
            _val = _val.add((26).toBigInteger())
        }
        decrypted = decrypted + chr(((_val.add((65).toBigInteger())).toInt()))
        i = i + 1
    }
    return decrypted
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Plaintext: " + plaintext)
        println("Encrypted: " + encrypted)
        println("Decrypted: " + decrypted)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
