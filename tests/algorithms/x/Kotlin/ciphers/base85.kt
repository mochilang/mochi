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

var ascii85_chars: String = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu"
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

fun ord(ch: String): Int {
    var idx: Int = ascii85_chars.indexOf(ch)
    if (idx >= 0) {
        return 33 + idx
    }
    return 0
}

fun chr(n: Int): String {
    if ((n >= 33) && (n <= 117)) {
        return ascii85_chars.substring(n - 33, n - 32)
    }
    return "?"
}

fun to_binary(n: Int, bits: Int): String {
    var b: String = ""
    var _val: Int = n
    while (_val > 0) {
        b = (Math.floorMod(_val, 2)).toString() + b
        _val = _val / 2
    }
    while (b.length < bits) {
        b = "0" + b
    }
    if (b.length == 0) {
        b = "0"
    }
    return b
}

fun bin_to_int(bits: String): Int {
    var n: Int = 0
    var i: Int = 0
    while (i < bits.length) {
        if (bits[i].toString() == "1") {
            n = (n * 2) + 1
        } else {
            n = n * 2
        }
        i = i + 1
    }
    return n
}

fun reverse(s: String): String {
    var res: String = ""
    var i: BigInteger = ((s.length - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        res = res + s[(i).toInt()].toString()
        i = i.subtract((1).toBigInteger())
    }
    return res
}

fun base10_to_85(d: Int): String {
    if (d > 0) {
        return chr((Math.floorMod(d, 85)) + 33) + base10_to_85(d / 85)
    }
    return ""
}

fun base85_to_10(digits: String): Int {
    var value: Int = 0
    var i: Int = 0
    while (i < digits.length) {
        value = (value * 85) + (ord(digits[i].toString()) - 33)
        i = i + 1
    }
    return value
}

fun ascii85_encode(data: String): String {
    var binary_data: String = ""
    for (_ch in data) {
        val ch = _ch.toString()
        binary_data = binary_data + to_binary(ord(ch), 8)
    }
    var null_values: BigInteger = ((((32 * ((binary_data.length / 32) + 1)) - binary_data.length) / 8).toBigInteger())
    var total_bits: BigInteger = ((32 * ((binary_data.length / 32) + 1)).toBigInteger())
    while ((binary_data.length).toBigInteger().compareTo((total_bits)) < 0) {
        binary_data = binary_data + "0"
    }
    var result: String = ""
    var i: Int = 0
    while (i < binary_data.length) {
        var chunk_bits: String = binary_data.substring(i, i + 32)
        var chunk_val: Int = bin_to_int(chunk_bits)
        var encoded: String = reverse(base10_to_85(chunk_val))
        result = result + encoded
        i = i + 32
    }
    if ((null_values.remainder((4).toBigInteger())).compareTo((0).toBigInteger()) != 0) {
        result = result.substring(0, ((result.length).toBigInteger().subtract((null_values))).toInt())
    }
    return result
}

fun ascii85_decode(data: String): String {
    var null_values: BigInteger = (((5 * ((data.length / 5) + 1)) - data.length).toBigInteger())
    var binary_data: String = data
    var i: Int = 0
    while ((i).toBigInteger().compareTo((null_values)) < 0) {
        binary_data = binary_data + "u"
        i = i + 1
    }
    var result: String = ""
    i = 0
    while (i < binary_data.length) {
        var chunk: String = binary_data.substring(i, i + 5)
        var value: Int = base85_to_10(chunk)
        var bits: String = to_binary(value, 32)
        var j: Int = 0
        while (j < 32) {
            var byte_bits: String = bits.substring(j, j + 8)
            var c: String = chr(bin_to_int(byte_bits))
            result = result + c
            j = j + 8
        }
        i = i + 5
    }
    var trim: BigInteger = null_values
    if ((null_values.remainder((5).toBigInteger())).compareTo((0).toBigInteger()) == 0) {
        trim = null_values.subtract((1).toBigInteger())
    }
    return result.substring(0, ((result.length).toBigInteger().subtract((trim))).toInt())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(ascii85_encode(""))
        println(ascii85_encode("12345"))
        println(ascii85_encode("base 85"))
        println(ascii85_decode(""))
        println(ascii85_decode("0etOA2#"))
        println(ascii85_decode("@UX=h+?24"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
