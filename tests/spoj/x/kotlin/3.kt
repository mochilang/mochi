val _dataDir = "/workspace/mochi/tests/spoj/x/mochi"

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
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

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

var sample_input: MutableList<String> = mutableListOf("1010110010 10110", "1110111011 10011")
fun is_substring(a: String, b: String): Int {
    var la: Int = (a.length).toInt()
    var lb: Int = (b.length).toInt()
    var i: Int = (0).toInt()
    while ((i + lb) <= la) {
        if (_sliceStr(a, i, i + lb) == b) {
            return 1
        }
        i = i + 1
    }
    return 0
}

fun solve(lines: MutableList<String>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    for (line in lines) {
        var parts: MutableList<String> = mutableListOf<String>()
        var cur: String = ""
        var i: Int = (0).toInt()
        while (i < line.length) {
            var ch: String = _sliceStr(line, i, i + 1)
            if (ch == " ") {
                parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
                cur = ""
            } else {
                cur = cur + ch
            }
            i = i + 1
        }
        parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
        var a: String = parts[0]!!
        var b: String = parts[1]!!
        res = run { val _tmp = res.toMutableList(); _tmp.add(is_substring(a, b)); _tmp }
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (r in solve(sample_input)) {
            println(r)
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
