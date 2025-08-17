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

var NUM_SQUARES: Int = (9).toInt()
var EMPTY_CELL: String = "."
var valid_board: MutableList<MutableList<String>> = mutableListOf(mutableListOf("5", "3", ".", ".", "7", ".", ".", ".", "."), mutableListOf("6", ".", ".", "1", "9", "5", ".", ".", "."), mutableListOf(".", "9", "8", ".", ".", ".", ".", "6", "."), mutableListOf("8", ".", ".", ".", "6", ".", ".", ".", "3"), mutableListOf("4", ".", ".", "8", ".", "3", ".", ".", "1"), mutableListOf("7", ".", ".", ".", "2", ".", ".", ".", "6"), mutableListOf(".", "6", ".", ".", ".", ".", "2", "8", "."), mutableListOf(".", ".", ".", "4", "1", "9", ".", ".", "5"), mutableListOf(".", ".", ".", ".", "8", ".", ".", "7", "9"))
var invalid_board: MutableList<MutableList<String>> = mutableListOf(mutableListOf("8", "3", ".", ".", "7", ".", ".", ".", "."), mutableListOf("6", ".", ".", "1", "9", "5", ".", ".", "."), mutableListOf(".", "9", "8", ".", ".", ".", ".", "6", "."), mutableListOf("8", ".", ".", ".", "6", ".", ".", ".", "3"), mutableListOf("4", ".", ".", "8", ".", "3", ".", ".", "1"), mutableListOf("7", ".", ".", ".", "2", ".", ".", ".", "6"), mutableListOf(".", "6", ".", ".", ".", ".", "2", "8", "."), mutableListOf(".", ".", ".", "4", "1", "9", ".", ".", "5"), mutableListOf(".", ".", ".", ".", "8", ".", ".", "7", "9"))
fun is_valid_sudoku_board(board: MutableList<MutableList<String>>): Boolean {
    if (board.size != NUM_SQUARES) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < NUM_SQUARES) {
        if ((board[i]!!).size != NUM_SQUARES) {
            return false
        }
        i = i + 1
    }
    var rows: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var cols: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var boxes: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    i = 0
    while (i < NUM_SQUARES) {
        rows = run { val _tmp = rows.toMutableList(); _tmp.add(mutableListOf<String>()); _tmp }
        cols = run { val _tmp = cols.toMutableList(); _tmp.add(mutableListOf<String>()); _tmp }
        boxes = run { val _tmp = boxes.toMutableList(); _tmp.add(mutableListOf<String>()); _tmp }
        i = i + 1
    }
    for (r in 0 until NUM_SQUARES) {
        for (c in 0 until NUM_SQUARES) {
            var value: String = ((board[r]!!) as MutableList<String>)[c]!!
            if (value == EMPTY_CELL) {
                continue
            }
            var box: Int = ((((r / 3).toInt()) * 3) + ((c / 3).toInt())).toInt()
            if ((((value in rows[r]!!) || (value in cols[c]!!) as Boolean)) || (value in boxes[box]!!)) {
                return false
            }
            _listSet(rows, r, run { val _tmp = (rows[r]!!).toMutableList(); _tmp.add(value); _tmp })
            _listSet(cols, c, run { val _tmp = (cols[c]!!).toMutableList(); _tmp.add(value); _tmp })
            _listSet(boxes, box, run { val _tmp = (boxes[box]!!).toMutableList(); _tmp.add(value); _tmp })
        }
    }
    return true
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(is_valid_sudoku_board(valid_board))
        println(is_valid_sudoku_board(invalid_board))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
