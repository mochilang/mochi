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

var ppmData: String = "P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n"
var img: MutableMap<String, Any?> = parsePpm(ppmData)
fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if ((neg as Boolean)) {
        n = 0 - n
    }
    return n
}

fun splitWs(s: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((((((ch == " ") || (ch == "\n") as Boolean)) || (ch == "\t") as Boolean)) || (ch == "\r")) {
            if (cur.length > 0) {
                parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
    }
    return parts
}

fun parsePpm(data: String): MutableMap<String, Any?> {
    var toks: MutableList<String> = splitWs(data)
    if (toks.size < 4) {
        return (mutableMapOf<String, Boolean>("err" to (true)) as MutableMap<String, Any?>)
    }
    var magic: String = toks[0]!!
    var w: Int = parseIntStr(toks[1]!!)
    var h: Int = parseIntStr(toks[2]!!)
    var maxv: Int = parseIntStr(toks[3]!!)
    var px: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 4
    while (i < toks.size) {
        px = run { val _tmp = px.toMutableList(); _tmp.add(parseIntStr(toks[i]!!)); _tmp }
        i = i + 1
    }
    return mutableMapOf<String, Any?>("magic" to (magic), "w" to (w), "h" to (h), "max" to (maxv), "px" to (px))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((("width=" + ((img)["w"]!!).toString()) + " height=") + ((img)["h"]!!).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
