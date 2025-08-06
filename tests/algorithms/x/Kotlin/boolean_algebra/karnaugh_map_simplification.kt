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

var kmap: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1), mutableListOf(1, 1))
fun row_string(row: MutableList<Int>): String {
    var s: String = "["
    var i: Int = 0
    while (i < row.size) {
        s = s + (row[i]!!).toString()
        if (i < (row.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun print_kmap(kmap: MutableList<MutableList<Int>>): Unit {
    var i: Int = 0
    while (i < kmap.size) {
        println(row_string(kmap[i]!!))
        i = i + 1
    }
}

fun join_terms(terms: MutableList<String>): String {
    if (terms.size == 0) {
        return ""
    }
    var res: String = terms[0]!!
    var i: Int = 1
    while (i < terms.size) {
        res = (res + " + ") + terms[i]!!
        i = i + 1
    }
    return res
}

fun simplify_kmap(board: MutableList<MutableList<Int>>): String {
    var terms: MutableList<String> = mutableListOf<String>()
    var a: Int = 0
    while (a < board.size) {
        var row: MutableList<Int> = board[a]!!
        var b: Int = 0
        while (b < row.size) {
            var item: Int = row[b]!!
            if (item != 0) {
                var term: String = ((if (a != 0) "A" else "A'" + if (b != 0) "B" else "B'") as String)
                terms = run { val _tmp = terms.toMutableList(); _tmp.add(term); _tmp }
            }
            b = b + 1
        }
        a = a + 1
    }
    var expr: String = join_terms(terms)
    return expr
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        print_kmap(kmap)
        println("Simplified Expression:")
        println(simplify_kmap(kmap))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
