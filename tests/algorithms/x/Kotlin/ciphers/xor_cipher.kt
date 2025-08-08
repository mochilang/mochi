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

var ascii: String = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
var sample: String = "hallo welt"
var enc: String = encrypt_string(sample, 1)
var dec: String = encrypt_string(enc, 1)
fun xor(a: Int, b: Int): Int {
    var res: Int = 0
    var bit: Int = 1
    var x: Int = a
    var y: Int = b
    while ((x > 0) || (y > 0)) {
        var abit: Int = Math.floorMod(x, 2)
        var bbit: Int = Math.floorMod(y, 2)
        if (abit != bbit) {
            res = res + bit
        }
        x = x / 2
        y = y / 2
        bit = bit * 2
    }
    return res
}

fun ord(ch: String): Int {
    var i: Int = 0
    while (i < ascii.length) {
        if (ascii.substring(i, i + 1) == ch) {
            return 32 + i
        }
        i = i + 1
    }
    return 0
}

fun chr(n: Int): String {
    if ((n >= 32) && (n < 127)) {
        return ascii.substring(n - 32, n - 31)
    }
    return ""
}

fun normalize_key(key: Int): Int {
    var k: Int = key
    if (k == 0) {
        k = 1
    }
    k = Math.floorMod(k, 256)
    if (k < 0) {
        k = k + 256
    }
    return k
}

fun encrypt(content: String, key: Int): MutableList<String> {
    var k: Int = normalize_key(key)
    var result: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < content.length) {
        var c: Int = ord(content.substring(i, i + 1))
        var e: Int = xor(c, k)
        result = run { val _tmp = result.toMutableList(); _tmp.add(chr(e)); _tmp }
        i = i + 1
    }
    return result
}

fun encrypt_string(content: String, key: Int): String {
    var chars: MutableList<String> = encrypt(content, key)
    var out: String = ""
    for (ch in chars) {
        out = out + ch
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encrypt(sample, 1).toString())
        println(enc)
        println(dec)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
