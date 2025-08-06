fun input(): String = readLine() ?: ""

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

fun charToNum(ch: String): Int {
    var letters: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = letters.indexOf(ch)
    if (idx >= 0) {
        return idx + 1
    }
    return 0
}

fun numToChar(n: Int): String {
    var letters: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 1) && (n <= 26)) {
        return letters.substring(n - 1, n)
    }
    return "?"
}

fun encode(plain: String): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < plain.length) {
        var ch: String = (plain.substring(i, i + 1).toLowerCase() as String)
        var _val: Int = charToNum(ch)
        if (_val > 0) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(_val); _tmp }
        }
        i = i + 1
    }
    return res
}

fun decode(encoded: MutableList<Int>): String {
    var out: String = ""
    for (n in encoded) {
        out = out + numToChar(n)
    }
    return out
}

fun user_main(): Unit {
    println("-> ")
    var text: String = (input().toLowerCase() as String)
    var enc: MutableList<Int> = encode(text)
    println("Encoded: " + enc.toString())
    println("Decoded: " + decode(enc))
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
