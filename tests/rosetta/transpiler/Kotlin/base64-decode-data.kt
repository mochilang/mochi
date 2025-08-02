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

var msg: String = "Rosetta Code Base64 decode data task"
fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str[0].toString() == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str[i].toString()] as Int
        i = i + 1
    }
    if ((neg as Boolean)) {
        n = 0 - n
    }
    return n
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = upper.indexOf(ch)
    if (idx >= 0) {
        return 65 + idx
    }
    idx = lower.indexOf(ch)
    if (idx >= 0) {
        return 97 + idx
    }
    if ((ch >= "0") && (ch <= "9")) {
        return 48 + parseIntStr(ch)
    }
    if (ch == "+") {
        return 43
    }
    if (ch == "/") {
        return 47
    }
    if (ch == " ") {
        return 32
    }
    if (ch == "=") {
        return 61
    }
    return 0
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 65) && (n < 91)) {
        return upper.substring(n - 65, n - 64)
    }
    if ((n >= 97) && (n < 123)) {
        return lower.substring(n - 97, n - 96)
    }
    if ((n >= 48) && (n < 58)) {
        var digits: String = "0123456789"
        return digits.substring(n - 48, n - 47)
    }
    if (n == 43) {
        return "+"
    }
    if (n == 47) {
        return "/"
    }
    if (n == 32) {
        return " "
    }
    if (n == 61) {
        return "="
    }
    return "?"
}

fun toBinary(n: Int, bits: Int): String {
    var b: String = ""
    var _val: Int = n
    var i: Int = 0
    while (i < bits) {
        b = (Math.floorMod(_val, 2)).toString() + b
        _val = ((_val / 2).toInt())
        i = i + 1
    }
    return b
}

fun binToInt(bits: String): Int {
    var n: Int = 0
    var i: Int = 0
    while (i < bits.length) {
        n = (n * 2) + parseIntStr(bits.substring(i, i + 1))
        i = i + 1
    }
    return n
}

fun base64Encode(text: String): String {
    var alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    var bin: String = ""
    for (_ch in text) {
        val ch = _ch.toString()
        bin = bin + toBinary(ord(ch), 8)
    }
    while ((Math.floorMod(bin.length, 6)) != 0) {
        bin = bin + "0"
    }
    var out: String = ""
    var i: Int = 0
    while (i < bin.length) {
        var chunk: String = bin.substring(i, i + 6)
        var _val: Int = binToInt(chunk)
        out = out + alphabet.substring(_val, _val + 1)
        i = i + 6
    }
    var pad: BigInteger = (Math.floorMod((3 - (Math.floorMod(text.length, 3))), 3)).toBigInteger()
    if (pad.compareTo((1).toBigInteger()) == 0) {
        out = out.substring(0, out.length - 1) + "="
    }
    if (pad.compareTo((2).toBigInteger()) == 0) {
        out = out.substring(0, out.length - 2) + "=="
    }
    return out
}

fun base64Decode(enc: String): String {
    var alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    var bin: String = ""
    var i: Int = 0
    while (i < enc.length) {
        var ch: String = enc[i].toString()
        if (ch == "=") {
            break
        }
        var idx: Int = alphabet.indexOf(ch)
        bin = bin + toBinary(idx, 6)
        i = i + 1
    }
    var out: String = ""
    i = 0
    while ((i + 8) <= bin.length) {
        var chunk: String = bin.substring(i, i + 8)
        var _val: Int = binToInt(chunk)
        out = out + chr(_val)
        i = i + 8
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Original : " + msg)
        var enc: String = base64Encode(msg)
        println("\nEncoded  : " + enc)
        var dec: String = base64Decode(enc)
        println("\nDecoded  : " + dec)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
