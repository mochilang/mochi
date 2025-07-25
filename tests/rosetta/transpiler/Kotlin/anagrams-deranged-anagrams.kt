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

fun deranged(a: String, b: String): Boolean {
    if (a.length != b.length) {
        return false
    }
    var i: Int = 0
    while (i < a.length) {
        if (a.substring(i, i + 1) == b.substring(i, i + 1)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun user_main(): Unit {
    val words: MutableList<String> = mutableListOf("constitutionalism", "misconstitutional")
    var m: MutableMap<String, MutableList<String>> = mutableMapOf<Any?, Any?>() as MutableMap<String, MutableList<String>>
    var bestLen: Int = 0
    var w1: String = ""
    var w2: String = ""
    for (w in words) {
        if (w.length <= bestLen) {
            continue
        }
        val k: String = sortRunes(w)
        if (!((k in m) as Boolean)) {
            (m)[k] = mutableListOf(w)
            continue
        }
        for (c in (m)[k] as MutableList<String>) {
            if ((deranged(w, c as String)) as Boolean) {
                bestLen = w.length
                w1 = c as String
                w2 = w
                break
            }
        }
        (m)[k] = run { val _tmp = ((m)[k] as MutableList<String>).toMutableList(); _tmp.add(w); _tmp }
    }
    println((((w1 + " ") + w2) + " : Length ") + bestLen.toString())
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
