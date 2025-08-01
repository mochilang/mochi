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

fun rotate(s: String, n: Int): String {
    return s.substring(n, s.length) + s.substring(0, n)
}

fun scrambleLeft(s: String): String {
    return ((s.substring(0, 1) + s.substring(2, 14)) + s.substring(1, 2)) + s.substring(14, s.length)
}

fun scrambleRight(s: String): String {
    return (((s.substring(1, 3) + s.substring(4, 15)) + s.substring(3, 4)) + s.substring(15, s.length)) + s.substring(0, 1)
}

fun chao(text: String, encode: Boolean): String {
    var left: String = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    var right: String = "PTLNBQDEOYSFAVZKGJRIHWXUMC"
    var out: String = ""
    var i: Int = 0
    while (i < text.length) {
        var ch: String = text.substring(i, i + 1)
        var idx: Int = 0
        if (encode as Boolean) {
            idx = right.indexOf(ch).toInt()
            out = out + left.substring(idx, idx + 1)
        } else {
            idx = left.indexOf(ch).toInt()
            out = out + right.substring(idx, idx + 1)
        }
        left = rotate(left, idx)
        right = rotate(right, idx)
        left = scrambleLeft(left)
        right = scrambleRight(right)
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var plain: String = "WELLDONEISBETTERTHANWELLSAID"
    var cipher: String = chao(plain, true)
    println(plain)
    println(cipher)
    println(chao(cipher, false))
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
