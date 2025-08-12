fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun depth_first_search(grid: MutableList<MutableList<Int>>, row: Int, col: Int, visit: MutableList<MutableList<Boolean>>): Int {
    var row_length: Int = (grid.size).toInt()
    var col_length: Int = ((grid[0]!!).size).toInt()
    if ((((((row < 0) || (col < 0) as Boolean)) || (row == row_length) as Boolean)) || (col == col_length)) {
        return 0
    }
    if ((((((visit[row]!!) as MutableList<Boolean>))[col]!!) as Boolean)) {
        return 0
    }
    if ((((grid[row]!!) as MutableList<Int>))[col]!! == 1) {
        return 0
    }
    if ((row == (row_length - 1)) && (col == (col_length - 1))) {
        return 1
    }
    _listSet(visit[row]!!, col, true)
    var count: Int = (0).toInt()
    count = count + depth_first_search(grid, row + 1, col, visit)
    count = count + depth_first_search(grid, row - 1, col, visit)
    count = count + depth_first_search(grid, row, col + 1, visit)
    count = count + depth_first_search(grid, row, col - 1, visit)
    _listSet(visit[row]!!, col, false)
    return count
}

fun count_paths(grid: MutableList<MutableList<Int>>): Int {
    var rows: Int = (grid.size).toInt()
    var cols: Int = ((grid[0]!!).size).toInt()
    var visit: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row_visit: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j < cols) {
            row_visit = run { val _tmp = row_visit.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        visit = run { val _tmp = visit.toMutableList(); _tmp.add(row_visit); _tmp }
        i = i + 1
    }
    return depth_first_search(grid, 0, 0, visit)
}

fun user_main(): Unit {
    var grid1: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 0, 0, 0), mutableListOf(1, 1, 0, 0), mutableListOf(0, 0, 0, 1), mutableListOf(0, 1, 0, 0))
    println(count_paths(grid1).toString())
    var grid2: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 0, 0, 0, 0), mutableListOf(0, 1, 1, 1, 0), mutableListOf(0, 1, 1, 1, 0), mutableListOf(0, 0, 0, 0, 0))
    println(count_paths(grid2).toString())
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
