fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
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

fun contains(xs: MutableList<Int>, x: Int): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun repeat(s: String, times: Int): String {
    var result: String = ""
    var i: Int = 0
    while (i < times) {
        result = result + s
        i = i + 1
    }
    return result
}

fun build_board(pos: MutableList<Int>, n: Int): MutableList<String> {
    var board: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < pos.size) {
        var col: Int = pos[i]!!
        var line: String = (repeat(". ", col) + "Q ") + repeat(". ", (n - 1) - col)
        board = run { val _tmp = board.toMutableList(); _tmp.add(line); _tmp }
        i = i + 1
    }
    return board
}

fun depth_first_search(pos: MutableList<Int>, dr: MutableList<Int>, dl: MutableList<Int>, n: Int): MutableList<MutableList<String>> {
    var row: Int = pos.size
    if (row == n) {
        var single: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
        single = run { val _tmp = single.toMutableList(); _tmp.add(build_board(pos, n)); _tmp }
        return single
    }
    var boards: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var col: Int = 0
    while (col < n) {
        if ((((pos.contains(col) as Boolean) || (dr.contains(row - col) as Boolean) as Boolean)) || (dl.contains(row + col) as Boolean)) {
            col = col + 1
            continue
        }
        var result: MutableList<MutableList<String>> = depth_first_search(run { val _tmp = pos.toMutableList(); _tmp.add(col); _tmp }, run { val _tmp = dr.toMutableList(); _tmp.add(row - col); _tmp }, run { val _tmp = dl.toMutableList(); _tmp.add(row + col); _tmp }, n)
        boards = ((concat(boards, result)) as MutableList<MutableList<String>>)
        col = col + 1
    }
    return boards
}

fun n_queens_solution(n: Int): Int {
    var boards: MutableList<MutableList<String>> = depth_first_search(mutableListOf<Int>(), mutableListOf<Int>(), mutableListOf<Int>(), n)
    var i: Int = 0
    while (i < boards.size) {
        var j: Int = 0
        while (j < (boards[i]!!).size) {
            println((((boards[i]!!) as MutableList<String>))[j]!!)
            j = j + 1
        }
        println("")
        i = i + 1
    }
    println(listOf(boards.size, "solutions were found.").joinToString(" "))
    return boards.size
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        n_queens_solution(4)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
