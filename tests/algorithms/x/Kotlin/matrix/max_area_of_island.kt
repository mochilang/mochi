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

var matrix: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0), mutableListOf(0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0), mutableListOf(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0))
fun encode(row: Int, col: Int): String {
    return (row.toString() + ",") + col.toString()
}

fun is_safe(row: Int, col: Int, rows: Int, cols: Int): Boolean {
    return (((((((row >= 0) && (row < rows) as Boolean)) && (col >= 0) as Boolean)) && (col < cols)) as Boolean)
}

fun has(seen: MutableMap<String, Boolean>, key: String): Boolean {
    return key in seen
}

fun depth_first_search(row: Int, col: Int, seen: MutableMap<String, Boolean>, mat: MutableList<MutableList<Int>>): Int {
    var rows: Int = (mat.size).toInt()
    var cols: Int = ((mat[0]!!).size).toInt()
    var key: String = encode(row, col)
    if (((is_safe(row, col, rows, cols) && (!has(seen, key) as Boolean) as Boolean)) && ((((mat[row]!!) as MutableList<Int>))[col]!! == 1)) {
        (seen)[key] = true
        return (((1 + depth_first_search(row + 1, col, seen, mat)) + depth_first_search(row - 1, col, seen, mat)) + depth_first_search(row, col + 1, seen, mat)) + depth_first_search(row, col - 1, seen, mat)
    } else {
        return 0
    }
}

fun find_max_area(mat: MutableList<MutableList<Int>>): Int {
    var seen: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
    var rows: Int = (mat.size).toInt()
    var max_area: Int = (0).toInt()
    var r: Int = (0).toInt()
    while (r < rows) {
        var line: MutableList<Int> = mat[r]!!
        var cols: Int = (line.size).toInt()
        var c: Int = (0).toInt()
        while (c < cols) {
            if (line[c]!! == 1) {
                var key: String = encode(r, c)
                if (!(key in seen)) {
                    var area: Int = (depth_first_search(r, c, seen, mat)).toInt()
                    if (area > max_area) {
                        max_area = area
                    }
                }
            }
            c = c + 1
        }
        r = r + 1
    }
    return max_area
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_max_area(matrix))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
