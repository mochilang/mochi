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

fun indexOfStr(h: String, n: String): Int {
    var hlen: Int = h.length
    var nlen: Int = n.length
    if (nlen == 0) {
        return 0
    }
    var i: Int = 0
    while (i <= (hlen - nlen)) {
        if (h.substring(i, i + nlen) == n) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun stringSearchSingle(h: String, n: String): Int {
    return indexOfStr(h, n)
}

fun stringSearch(h: String, n: String): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var start: Int = 0
    var hlen: Int = h.length
    var nlen: Int = n.length
    while (start < hlen) {
        var idx: Int = indexOfStr(h.substring(start, hlen), n)
        if (idx >= 0) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(start + idx); _tmp }
            start = (start + idx) + nlen
        } else {
            break
        }
    }
    return result
}

fun display(nums: MutableList<Int>): String {
    var s: String = "["
    var i: Int = 0
    while (i < nums.size) {
        if (i > 0) {
            s = s + ", "
        }
        s = s + (nums[i]!!).toString()
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    var texts: MutableList<String> = mutableListOf("GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk.")
    var patterns: MutableList<String> = mutableListOf("TCTA", "TAATAAA", "word", "needle", "and", "alfalfa")
    var i: Int = 0
    while (i < texts.size) {
        println((("text" + (i + 1).toString()) + " = ") + texts[i]!!)
        i = i + 1
    }
    println("")
    var j: Int = 0
    while (j < texts.size) {
        var idxs: MutableList<Int> = stringSearch(texts[j]!!, patterns[j]!!)
        println((((("Found \"" + patterns[j]!!) + "\" in 'text") + (j + 1).toString()) + "' at indexes ") + display(idxs))
        j = j + 1
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
