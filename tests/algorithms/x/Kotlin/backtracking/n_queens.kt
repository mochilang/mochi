import java.math.BigInteger

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

fun create_board(n: Int): MutableList<MutableList<Int>> {
    var board: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        board = run { val _tmp = board.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return board
}

fun is_safe(board: MutableList<MutableList<Int>>, row: Int, column: Int): Boolean {
    var n: Int = board.size
    var i: Int = 0
    while (i < row) {
        if ((((board[i]!!) as MutableList<Int>))[column]!! == 1) {
            return false
        }
        i = i + 1
    }
    i = row - 1
    var j: BigInteger = ((column - 1).toBigInteger())
    while ((i >= 0) && (j.compareTo((0).toBigInteger()) >= 0)) {
        if ((((board[i]!!) as MutableList<Int>))[(j).toInt()]!! == 1) {
            return false
        }
        i = i - 1
        j = j.subtract((1).toBigInteger())
    }
    i = row - 1
    j = ((column + 1).toBigInteger())
    while ((i >= 0) && (j.compareTo((n).toBigInteger()) < 0)) {
        if ((((board[i]!!) as MutableList<Int>))[(j).toInt()]!! == 1) {
            return false
        }
        i = i - 1
        j = j.add((1).toBigInteger())
    }
    return true
}

fun row_string(row: MutableList<Int>): String {
    var s: String = ""
    var j: Int = 0
    while (j < row.size) {
        if (row[j]!! == 1) {
            s = s + "Q "
        } else {
            s = s + ". "
        }
        j = j + 1
    }
    return s
}

fun printboard(board: MutableList<MutableList<Int>>): Unit {
    var i: Int = 0
    while (i < board.size) {
        println(row_string(board[i]!!))
        i = i + 1
    }
}

fun solve(board: MutableList<MutableList<Int>>, row: Int): Int {
    if (row >= board.size) {
        printboard(board)
        println("")
        return 1
    }
    var count: Int = 0
    var i: Int = 0
    while (i < board.size) {
        if (((is_safe(board, row, i)) as Boolean)) {
            (board[row]!!)[i] = 1
            count = count + solve(board, row + 1)
            (board[row]!!)[i] = 0
        }
        i = i + 1
    }
    return count
}

fun n_queens(n: Int): Int {
    var board: MutableList<MutableList<Int>> = create_board(n)
    var total: Int = solve(board, 0)
    println("The total number of solutions are: " + total.toString())
    return total
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        n_queens(4)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
