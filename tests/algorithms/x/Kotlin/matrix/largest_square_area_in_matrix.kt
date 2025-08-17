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

var sample: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1), mutableListOf(1, 1))
fun update_area_of_max_square(row: Int, col: Int, rows: Int, cols: Int, mat: MutableList<MutableList<Int>>, largest_square_area: MutableList<Int>): Int {
    if ((row >= rows) || (col >= cols)) {
        return 0
    }
    var right: Int = (update_area_of_max_square(row, col + 1, rows, cols, mat, largest_square_area)).toInt()
    var diagonal: Int = (update_area_of_max_square(row + 1, col + 1, rows, cols, mat, largest_square_area)).toInt()
    var down: Int = (update_area_of_max_square(row + 1, col, rows, cols, mat, largest_square_area)).toInt()
    if (((mat[row]!!) as MutableList<Int>)[col]!! == 1) {
        var sub: Int = (1 + mutableListOf(right, diagonal, down).min()!!).toInt()
        if (sub > largest_square_area[0]!!) {
            _listSet(largest_square_area, 0, sub)
        }
        return sub
    } else {
        return 0
    }
}

fun largest_square_area_in_matrix_top_down(rows: Int, cols: Int, mat: MutableList<MutableList<Int>>): Int {
    var largest: MutableList<Int> = mutableListOf(0)
    update_area_of_max_square(0, 0, rows, cols, mat, largest)
    return largest[0]!!
}

fun update_area_of_max_square_with_dp(row: Int, col: Int, rows: Int, cols: Int, mat: MutableList<MutableList<Int>>, dp_array: MutableList<MutableList<Int>>, largest_square_area: MutableList<Int>): Int {
    if ((row >= rows) || (col >= cols)) {
        return 0
    }
    if (((dp_array[row]!!) as MutableList<Int>)[col]!! != (0 - 1)) {
        return ((dp_array[row]!!) as MutableList<Int>)[col]!!
    }
    var right: Int = (update_area_of_max_square_with_dp(row, col + 1, rows, cols, mat, dp_array, largest_square_area)).toInt()
    var diagonal: Int = (update_area_of_max_square_with_dp(row + 1, col + 1, rows, cols, mat, dp_array, largest_square_area)).toInt()
    var down: Int = (update_area_of_max_square_with_dp(row + 1, col, rows, cols, mat, dp_array, largest_square_area)).toInt()
    if (((mat[row]!!) as MutableList<Int>)[col]!! == 1) {
        var sub: Int = (1 + mutableListOf(right, diagonal, down).min()!!).toInt()
        if (sub > largest_square_area[0]!!) {
            _listSet(largest_square_area, 0, sub)
        }
        _listSet(dp_array[row]!!, col, sub)
        return sub
    } else {
        _listSet(dp_array[row]!!, col, 0)
        return 0
    }
}

fun largest_square_area_in_matrix_top_down_with_dp(rows: Int, cols: Int, mat: MutableList<MutableList<Int>>): Int {
    var largest: MutableList<Int> = mutableListOf(0)
    var dp_array: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var r: Int = (0).toInt()
    while (r < rows) {
        var row_list: MutableList<Int> = mutableListOf<Int>()
        var c: Int = (0).toInt()
        while (c < cols) {
            row_list = run { val _tmp = row_list.toMutableList(); _tmp.add(0 - 1); _tmp }
            c = c + 1
        }
        dp_array = run { val _tmp = dp_array.toMutableList(); _tmp.add(row_list); _tmp }
        r = r + 1
    }
    update_area_of_max_square_with_dp(0, 0, rows, cols, mat, dp_array, largest)
    return largest[0]!!
}

fun largest_square_area_in_matrix_bottom_up(rows: Int, cols: Int, mat: MutableList<MutableList<Int>>): Int {
    var dp_array: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var r: Int = (0).toInt()
    while (r <= rows) {
        var row_list: MutableList<Int> = mutableListOf<Int>()
        var c: Int = (0).toInt()
        while (c <= cols) {
            row_list = run { val _tmp = row_list.toMutableList(); _tmp.add(0); _tmp }
            c = c + 1
        }
        dp_array = run { val _tmp = dp_array.toMutableList(); _tmp.add(row_list); _tmp }
        r = r + 1
    }
    var largest: Int = (0).toInt()
    var row: Int = (rows - 1).toInt()
    while (row >= 0) {
        var col: Int = (cols - 1).toInt()
        while (col >= 0) {
            var right: Int = (((dp_array[row]!!) as MutableList<Int>)[col + 1]!!).toInt()
            var diagonal: Int = (((dp_array[row + 1]!!) as MutableList<Int>)[col + 1]!!).toInt()
            var bottom: Int = (((dp_array[row + 1]!!) as MutableList<Int>)[col]!!).toInt()
            if (((mat[row]!!) as MutableList<Int>)[col]!! == 1) {
                var value: Int = (1 + mutableListOf(right, diagonal, bottom).min()!!).toInt()
                _listSet(dp_array[row]!!, col, value)
                if (value > largest) {
                    largest = value
                }
            } else {
                _listSet(dp_array[row]!!, col, 0)
            }
            col = col - 1
        }
        row = row - 1
    }
    return largest
}

fun largest_square_area_in_matrix_bottom_up_space_optimization(rows: Int, cols: Int, mat: MutableList<MutableList<Int>>): Int {
    var current_row: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= cols) {
        current_row = run { val _tmp = current_row.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var next_row: MutableList<Int> = mutableListOf<Int>()
    var j: Int = (0).toInt()
    while (j <= cols) {
        next_row = run { val _tmp = next_row.toMutableList(); _tmp.add(0); _tmp }
        j = j + 1
    }
    var largest: Int = (0).toInt()
    var row: Int = (rows - 1).toInt()
    while (row >= 0) {
        var col: Int = (cols - 1).toInt()
        while (col >= 0) {
            var right: Int = (current_row[col + 1]!!).toInt()
            var diagonal: Int = (next_row[col + 1]!!).toInt()
            var bottom: Int = (next_row[col]!!).toInt()
            if (((mat[row]!!) as MutableList<Int>)[col]!! == 1) {
                var value: Int = (1 + mutableListOf(right, diagonal, bottom).min()!!).toInt()
                _listSet(current_row, col, value)
                if (value > largest) {
                    largest = value
                }
            } else {
                _listSet(current_row, col, 0)
            }
            col = col - 1
        }
        next_row = current_row
        current_row = mutableListOf<Int>()
        var t: Int = (0).toInt()
        while (t <= cols) {
            current_row = run { val _tmp = current_row.toMutableList(); _tmp.add(0); _tmp }
            t = t + 1
        }
        row = row - 1
    }
    return largest
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(largest_square_area_in_matrix_top_down(2, 2, sample))
        println(largest_square_area_in_matrix_top_down_with_dp(2, 2, sample))
        println(largest_square_area_in_matrix_bottom_up(2, 2, sample))
        println(largest_square_area_in_matrix_bottom_up_space_optimization(2, 2, sample))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
