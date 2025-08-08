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

var example1: String = "AAAABBBCCDAA"
var encoded1: String = run_length_encode(example1)
fun run_length_encode(text: String): String {
    if (text.length == 0) {
        return ""
    }
    var encoded: String = ""
    var count: Int = 1
    var i: Int = 0
    while (i < text.length) {
        if (((i + 1) < text.length) && (text[i].toString() == text[i + 1].toString())) {
            count = count + 1
        } else {
            encoded = (encoded + text[i].toString()) + count.toString()
            count = 1
        }
        i = i + 1
    }
    return encoded
}

fun run_length_decode(encoded: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < encoded.length) {
        var ch: String = encoded[i].toString()
        i = i + 1
        var num_str: String = ""
        while ((((i < encoded.length) && (encoded[i].toString() >= "0") as Boolean)) && (encoded[i].toString() <= "9")) {
            num_str = num_str + encoded[i].toString()
            i = i + 1
        }
        var count: Int = (num_str.toBigInteger().toInt())
        var j: Int = 0
        while (j < count) {
            res = res + ch
            j = j + 1
        }
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encoded1)
        println(run_length_decode(encoded1))
        var example2: String = "A"
        var encoded2: String = run_length_encode(example2)
        println(encoded2)
        println(run_length_decode(encoded2))
        var example3: String = "AAADDDDDDFFFCCCAAVVVV"
        var encoded3: String = run_length_encode(example3)
        println(encoded3)
        println(run_length_decode(encoded3))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
