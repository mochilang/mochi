import java.math.BigInteger

fun json(v: Any?) { println(toJson(v)) }

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
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

var B64_CHARSET: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
fun to_binary(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var num: Int = n
    var res: String = ""
    while (num > 0) {
        var bit: Int = Math.floorMod(num, 2)
        res = bit.toString() + res
        num = num / 2
    }
    return res
}

fun zfill(s: String, width: Int): String {
    var res: String = s
    var pad: BigInteger = ((width - s.length).toBigInteger())
    while (pad.compareTo((0).toBigInteger()) > 0) {
        res = "0" + res
        pad = pad.subtract((1).toBigInteger())
    }
    return res
}

fun from_binary(s: String): Int {
    var i: Int = 0
    var result: Int = 0
    while (i < s.length) {
        result = result * 2
        if (s.substring(i, i + 1) == "1") {
            result = result + 1
        }
        i = i + 1
    }
    return result
}

fun repeat(ch: String, times: Int): String {
    var res: String = ""
    var i: Int = 0
    while (i < times) {
        res = res + ch
        i = i + 1
    }
    return res
}

fun char_index(s: String, c: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == c) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun base64_encode(data: MutableList<Int>): String {
    var bits: String = ""
    var i: Int = 0
    while (i < data.size) {
        bits = bits + zfill(to_binary(data[i]!!), 8)
        i = i + 1
    }
    var pad_bits: Int = 0
    if ((Math.floorMod(bits.length, 6)) != 0) {
        pad_bits = 6 - (Math.floorMod(bits.length, 6))
        bits = bits + repeat("0", pad_bits)
    }
    var j: Int = 0
    var encoded: String = ""
    while (j < bits.length) {
        var chunk: String = bits.substring(j, j + 6)
        var idx: Int = from_binary(chunk)
        encoded = encoded + B64_CHARSET.substring(idx, idx + 1)
        j = j + 6
    }
    var pad: Int = pad_bits / 2
    while (pad > 0) {
        encoded = encoded + "="
        pad = pad - 1
    }
    return encoded
}

fun base64_decode(s: String): MutableList<Int> {
    var padding: Int = 0
    var end: Int = s.length
    while ((end > 0) && (s.substring(end - 1, end) == "=")) {
        padding = padding + 1
        end = end - 1
    }
    var bits: String = ""
    var k: Int = 0
    while (k < end) {
        var c: String = s.substring(k, k + 1)
        var idx: Int = char_index(B64_CHARSET, c)
        bits = bits + zfill(to_binary(idx), 6)
        k = k + 1
    }
    if (padding > 0) {
        bits = bits.substring(0, bits.length - (padding * 2))
    }
    var bytes: MutableList<Int> = mutableListOf<Int>()
    var m: Int = 0
    while (m < bits.length) {
        var byte: Int = from_binary(bits.substring(m, m + 8))
        bytes = run { val _tmp = bytes.toMutableList(); _tmp.add(byte); _tmp }
        m = m + 8
    }
    return bytes
}

fun user_main(): Unit {
    var data: MutableList<Int> = mutableListOf(77, 111, 99, 104, 105)
    var encoded: String = base64_encode(data)
    println(encoded)
    json(((base64_decode(encoded)) as Any?))
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
