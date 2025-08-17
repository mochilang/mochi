import java.math.BigInteger

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
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

var grid: MutableList<MutableList<Int>> = generate_large_matrix()
var test_grids: MutableList<MutableList<MutableList<Int>>> = mutableListOf(mutableListOf(mutableListOf(4, 3, 2, 0 - 1), mutableListOf(3, 2, 1, 0 - 1), mutableListOf(1, 1, 0 - 1, 0 - 2), mutableListOf(0 - 1, 0 - 1, 0 - 2, 0 - 3)), mutableListOf(mutableListOf(3, 2), mutableListOf(1, 0)), mutableListOf(mutableListOf(7, 7, 6)), mutableListOf(mutableListOf(7, 7, 6), mutableListOf(0 - 1, 0 - 2, 0 - 3)), grid)
var results_bin: MutableList<Int> = mutableListOf<Int>()
var i: Int = (0).toInt()
fun generate_large_matrix(): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < 1000) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (1000 - i).toInt()
        while (j > ((0 - 1000) - i)) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(j); _tmp }
            j = j - 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = (i + 1).toInt()
    }
    return result
}

fun find_negative_index(arr: MutableList<Int>): Int {
    var left: Int = (0).toInt()
    var right: Int = (arr.size - 1).toInt()
    if (arr.size == 0) {
        return 0
    }
    if (arr[0]!! < 0) {
        return 0
    }
    while (left <= right) {
        var mid: Int = ((left + right) / 2).toInt()
        var num: Int = (arr[mid]!!).toInt()
        if (num < 0) {
            if (mid == 0) {
                return 0
            }
            if (arr[mid - 1]!! >= 0) {
                return mid
            }
            right = mid - 1
        } else {
            left = mid + 1
        }
    }
    return arr.size
}

fun count_negatives_binary_search(grid: MutableList<MutableList<Int>>): Int {
    var total: Int = (0).toInt()
    var bound: Int = ((grid[0]!!).size).toInt()
    var i: Int = (0).toInt()
    while (i < grid.size) {
        var row: MutableList<Int> = grid[i]!!
        var idx: Int = (find_negative_index(_sliceList(row, 0, bound))).toInt()
        bound = idx
        total = total + idx
        i = (i + 1).toInt()
    }
    return (grid.size * (grid[0]!!).size) - total
}

fun count_negatives_brute_force(grid: MutableList<MutableList<Int>>): Int {
    var count: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < grid.size) {
        var row: MutableList<Int> = grid[i]!!
        var j: Int = (0).toInt()
        while (j < row.size) {
            if (row[j]!! < 0) {
                count = count + 1
            }
            j = j + 1
        }
        i = (i + 1).toInt()
    }
    return count
}

fun count_negatives_brute_force_with_break(grid: MutableList<MutableList<Int>>): Int {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < grid.size) {
        var row: MutableList<Int> = grid[i]!!
        var j: Int = (0).toInt()
        while (j < row.size) {
            var number: Int = (row[j]!!).toInt()
            if (number < 0) {
                total = total + (row.size - j)
                break
            }
            j = j + 1
        }
        i = (i + 1).toInt()
    }
    return total
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < test_grids.size) {
            results_bin = run { val _tmp = results_bin.toMutableList(); _tmp.add(count_negatives_binary_search(test_grids[i]!!)); _tmp }
            i = (i + 1).toInt()
        }
        println(results_bin.toString())
        var results_brute: MutableList<Int> = mutableListOf<Int>()
        i = (0).toInt()
        while (i < test_grids.size) {
            results_brute = run { val _tmp = results_brute.toMutableList(); _tmp.add(count_negatives_brute_force(test_grids[i]!!)); _tmp }
            i = (i + 1).toInt()
        }
        println(results_brute.toString())
        var results_break: MutableList<Int> = mutableListOf<Int>()
        i = (0).toInt()
        while (i < test_grids.size) {
            results_break = run { val _tmp = results_break.toMutableList(); _tmp.add(count_negatives_brute_force_with_break(test_grids[i]!!)); _tmp }
            i = (i + 1).toInt()
        }
        println(results_break.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
