import java.math.BigInteger

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

var grid: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1, 0, 0, 0), mutableListOf(0, 1, 0, 0, 1), mutableListOf(1, 0, 0, 1, 1), mutableListOf(0, 0, 0, 0, 0), mutableListOf(1, 0, 1, 0, 1))
fun is_safe(grid: MutableList<MutableList<Int>>, visited: MutableList<MutableList<Boolean>>, row: Int, col: Int): Boolean {
    var rows: Int = (grid.size).toInt()
    var cols: Int = ((grid[0]!!).size).toInt()
    var within_bounds: Boolean = ((((((row >= 0) && (row < rows) as Boolean)) && (col >= 0) as Boolean)) && (col < cols)) as Boolean
    if (!within_bounds) {
        return false
    }
    var visited_cell: Boolean = ((visited[row]!!) as MutableList<Boolean>)[col]!!
    var not_visited: Boolean = visited_cell == false
    return (not_visited && (((grid[row]!!) as MutableList<Int>)[col]!! == 1)) as Boolean
}

fun dfs(grid: MutableList<MutableList<Int>>, visited: MutableList<MutableList<Boolean>>, row: Int, col: Int): Unit {
    var row_nbr: MutableList<Int> = mutableListOf(0 - 1, 0 - 1, 0 - 1, 0, 0, 1, 1, 1)
    var col_nbr: MutableList<Int> = mutableListOf(0 - 1, 0, 1, 0 - 1, 1, 0 - 1, 0, 1)
    _listSet(visited[row]!!, col, true)
    var k: Int = (0).toInt()
    while (k < 8) {
        var new_row: Int = (row + row_nbr[k]!!).toInt()
        var new_col: Int = (col + col_nbr[k]!!).toInt()
        if ((is_safe(grid, visited, new_row, new_col)) as Boolean) {
            dfs(grid, visited, new_row, new_col)
        }
        k = k + 1
    }
}

fun count_islands(grid: MutableList<MutableList<Int>>): Int {
    var rows: Int = (grid.size).toInt()
    var cols: Int = ((grid[0]!!).size).toInt()
    var visited: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row_list: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j < cols) {
            row_list = run { val _tmp = row_list.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        visited = run { val _tmp = visited.toMutableList(); _tmp.add(row_list); _tmp }
        i = i + 1
    }
    var count: Int = (0).toInt()
    i = 0
    while (i < rows) {
        var j: Int = (0).toInt()
        while (j < cols) {
            if ((!((((visited[i]!!) as MutableList<Boolean>)[j]!!) as? Boolean ?: false) as Boolean) && (((grid[i]!!) as MutableList<Int>)[j]!! == 1)) {
                dfs(grid, visited, i, j)
                count = count + 1
            }
            j = j + 1
        }
        i = i + 1
    }
    return count
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(count_islands(grid))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
