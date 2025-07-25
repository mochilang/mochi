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

fun sortRunes(s: String): String {
    var arr: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < s.length) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(s.substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    var n: Int = arr.size
    var m: Int = 0
    while (m < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            if (arr[j] > arr[j + 1]) {
                val tmp: String = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = tmp
            }
            j = j + 1
        }
        m = m + 1
    }
    var out: String = ""
    i = 0
    while (i < n) {
        out = out + arr[i]
        i = i + 1
    }
    return out
}

fun sortStrings(xs: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf()
    var tmp: MutableList<String> = xs
    while (tmp.size > 0) {
        var min: String = tmp[0]
        var idx: Int = 0
        var i: Int = 1
        while (i < tmp.size) {
            if (tmp[i] < min) {
                min = tmp[i]
                idx = i
            }
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(min); _tmp } as MutableList<String>
        var out: MutableList<String> = mutableListOf()
        var j: Int = 0
        while (j < tmp.size) {
            if (j != idx) {
                out = run { val _tmp = out.toMutableList(); _tmp.add(tmp[j]); _tmp } as MutableList<String>
            }
            j = j + 1
        }
        tmp = out
    }
    return res
}

fun user_main(): Unit {
    val words: MutableList<String> = mutableListOf("abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile")
    var groups: MutableMap<String, MutableList<String>> = mutableMapOf<Any?, Any?>() as MutableMap<String, MutableList<String>>
    var maxLen: Int = 0
    for (w in words) {
        val k: String = sortRunes(w)
        if (!((k in groups) as Boolean)) {
            (groups)[k] = mutableListOf(w)
        } else {
            (groups)[k] = run { val _tmp = ((groups)[k] as MutableList<String>).toMutableList(); _tmp.add(w); _tmp }
        }
        if (((groups)[k] as MutableList<String>).size > maxLen) {
            maxLen = ((groups)[k] as MutableList<String>).size
        }
    }
    var printed: MutableMap<String, Boolean> = mutableMapOf<Any?, Any?>() as MutableMap<String, Boolean>
    for (w in words) {
        val k: String = sortRunes(w)
        if (((groups)[k] as MutableList<String>).size == maxLen) {
            if (!((k in printed) as Boolean)) {
                var g: MutableList<String> = sortStrings((groups)[k] as MutableList<String>)
                var line: String = "[" + g[0]
                var i: Int = 1
                while (i < g.size) {
                    line = (line + " ") + g[i]
                    i = i + 1
                }
                line = line + "]"
                println(line)
                (printed)[k] = true
            }
        }
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
