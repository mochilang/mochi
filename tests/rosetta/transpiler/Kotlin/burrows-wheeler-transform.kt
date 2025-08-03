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

var stx: String = "\\u0002"
var etx: String = "\\u0003"
fun contains(s: String, ch: String): Boolean {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return true
        }
        i = i + 1
    }
    return false
}

fun sortStrings(xs: MutableList<String>): MutableList<String> {
    var arr: MutableList<String> = xs
    var n: Int = arr.size
    var i: Int = 0
    while (i < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            if (arr[j]!! > arr[j + 1]!!) {
                var tmp: String = arr[j]!!
                arr[j] = arr[j + 1]!!
                arr[j + 1] = tmp
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun bwt(s: String): MutableMap<String, Any?> {
    var s: String = s
    if ((s.contains(stx) as Boolean) || (s.contains(etx) as Boolean)) {
        return mutableMapOf<String, Any?>("err" to (true), "res" to (""))
    }
    s = (stx + s) + etx
    var le: Int = s.length
    var table: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < le) {
        var rot: String = s.substring(i, le) + s.substring(0, i)
        table = run { val _tmp = table.toMutableList(); _tmp.add(rot); _tmp }
        i = i + 1
    }
    table = sortStrings(table)
    var last: String = ""
    i = 0
    while (i < le) {
        last = last + table[i]!!.substring(le - 1, le)
        i = i + 1
    }
    return mutableMapOf<String, Any?>("err" to (false), "res" to (last))
}

fun ibwt(r: String): String {
    var le: Int = r.length
    var table: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < le) {
        table = run { val _tmp = table.toMutableList(); _tmp.add(""); _tmp }
        i = i + 1
    }
    var n: Int = 0
    while (n < le) {
        i = 0
        while (i < le) {
            table[i] = r.substring(i, i + 1) + table[i]!!
            i = i + 1
        }
        table = sortStrings(table)
        n = n + 1
    }
    i = 0
    while (i < le) {
        if (table[i]!!.substring(le - 1, le) == etx) {
            return table[i]!!.substring(1, le - 1)
        }
        i = i + 1
    }
    return ""
}

fun makePrintable(s: String): String {
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == stx) {
            out = out + "^"
        } else {
            if (ch == etx) {
                out = out + "|"
            } else {
                out = out + ch
            }
        }
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var examples: MutableList<String> = mutableListOf("banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\\u0002ABC\\u0003")
    for (t in examples) {
        println(makePrintable(t))
        var res: MutableMap<String, Any?> = bwt(t)
        if ((((res)["err"] as Any?) as Boolean)) {
            println(" --> ERROR: String can't contain STX or ETX")
            println(" -->")
        } else {
            var enc: String = (res)["res"] as String
            println(" --> " + makePrintable(enc))
            var r: String = ibwt(enc)
            println(" --> " + r)
        }
        println("")
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
