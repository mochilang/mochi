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

fun amb(wordsets: MutableList<MutableList<String>>, res: MutableList<String>, idx: Int): Boolean {
    if (idx == wordsets.size) {
        return true
    }
    var prev: String = ""
    if (idx > 0) {
        prev = res[idx - 1]
    }
    var i: Int = 0
    while (i < wordsets[idx].size) {
        val w: String = wordsets[idx][i]
        if ((idx == 0) || (prev.substring(prev.length - 1, prev.length) == w.substring(0, 1))) {
            res[idx] = w
            if ((amb(wordsets, res, idx + 1)) as Boolean) {
                return true
            }
        }
        i = i + 1
    }
    return false
}

fun user_main(): Unit {
    val wordset: MutableList<MutableList<String>> = mutableListOf(mutableListOf("the", "that", "a"), mutableListOf("frog", "elephant", "thing"), mutableListOf("walked", "treaded", "grows"), mutableListOf("slowly", "quickly"))
    var res: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < wordset.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(""); _tmp } as MutableList<String>
        i = i + 1
    }
    if ((amb(wordset, res, 0)) as Boolean) {
        var out: String = "[" + res[0]
        var j: Int = 1
        while (j < res.size) {
            out = (out + " ") + res[j]
            j = j + 1
        }
        out = out + "]"
        println(out)
    } else {
        println("No amb found")
    }
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
