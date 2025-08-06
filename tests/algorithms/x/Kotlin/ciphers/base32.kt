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

var B32_CHARSET: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
fun indexOfChar(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var digits: String = "0123456789"
    var idx: Int = indexOfChar(upper, ch)
    if (idx >= 0) {
        return 65 + idx
    }
    idx = indexOfChar(lower, ch)
    if (idx >= 0) {
        return 97 + idx
    }
    idx = indexOfChar(digits, ch)
    if (idx >= 0) {
        return 48 + idx
    }
    if (ch == " ") {
        return 32
    }
    if (ch == "!") {
        return 33
    }
    return 0
}

fun chr(code: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var digits: String = "0123456789"
    if (code == 32) {
        return " "
    }
    if (code == 33) {
        return "!"
    }
    var idx: BigInteger = ((code - 65).toBigInteger())
    if ((idx.compareTo((0).toBigInteger()) >= 0) && (idx.compareTo((upper.length).toBigInteger()) < 0)) {
        return upper[(idx).toInt()].toString()
    }
    idx = ((code - 97).toBigInteger())
    if ((idx.compareTo((0).toBigInteger()) >= 0) && (idx.compareTo((lower.length).toBigInteger()) < 0)) {
        return lower[(idx).toInt()].toString()
    }
    idx = ((code - 48).toBigInteger())
    if ((idx.compareTo((0).toBigInteger()) >= 0) && (idx.compareTo((digits.length).toBigInteger()) < 0)) {
        return digits[(idx).toInt()].toString()
    }
    return ""
}

fun repeat(s: String, n: Int): String {
    var out: String = ""
    var i: Int = 0
    while (i < n) {
        out = out + s
        i = i + 1
    }
    return out
}

fun to_binary(n: Int, bits: Int): String {
    var v: Int = n
    var out: String = ""
    var i: Int = 0
    while (i < bits) {
        out = (Math.floorMod(v, 2)).toString() + out
        v = v / 2
        i = i + 1
    }
    return out
}

fun binary_to_int(bits: String): Int {
    var n: Int = 0
    var i: Int = 0
    while (i < bits.length) {
        n = n * 2
        if (bits[i].toString() == "1") {
            n = n + 1
        }
        i = i + 1
    }
    return n
}

fun base32_encode(data: String): String {
    var binary_data: String = ""
    var i: Int = 0
    while (i < data.length) {
        binary_data = binary_data + to_binary(ord(data[i].toString()), 8)
        i = i + 1
    }
    var remainder: Int = Math.floorMod(binary_data.length, 5)
    if (remainder != 0) {
        binary_data = binary_data + repeat("0", 5 - remainder)
    }
    var b32_result: String = ""
    var j: Int = 0
    while (j < binary_data.length) {
        var chunk: String = binary_data.substring(j, j + 5)
        var index: Int = binary_to_int(chunk)
        b32_result = b32_result + B32_CHARSET[index].toString()
        j = j + 5
    }
    var rem: Int = Math.floorMod(b32_result.length, 8)
    if (rem != 0) {
        b32_result = b32_result + repeat("=", 8 - rem)
    }
    return b32_result
}

fun base32_decode(data: String): String {
    var clean: String = ""
    var i: Int = 0
    while (i < data.length) {
        var ch: String = data[i].toString()
        if (ch != "=") {
            clean = clean + ch
        }
        i = i + 1
    }
    var binary_chunks: String = ""
    i = 0
    while (i < clean.length) {
        var idx: Int = indexOfChar(B32_CHARSET, clean[i].toString())
        binary_chunks = binary_chunks + to_binary(idx, 5)
        i = i + 1
    }
    var result: String = ""
    var j: Int = 0
    while ((j + 8) <= binary_chunks.length) {
        var byte_bits: String = binary_chunks.substring(j, j + 8)
        var code: Int = binary_to_int(byte_bits)
        result = result + chr(code)
        j = j + 8
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(base32_encode("Hello World!"))
        println(base32_encode("123456"))
        println(base32_encode("some long complex string"))
        println(base32_decode("JBSWY3DPEBLW64TMMQQQ===="))
        println(base32_decode("GEZDGNBVGY======"))
        println(base32_decode("ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
