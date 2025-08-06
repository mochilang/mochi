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

var glider: MutableList<MutableList<Boolean>> = mutableListOf(mutableListOf(false, true, false, false, false), mutableListOf(false, false, true, false, false), mutableListOf(true, true, true, false, false), mutableListOf(false, false, false, false, false), mutableListOf(false, false, false, false, false))
var board: MutableList<MutableList<Boolean>> = glider
fun count_alive_neighbours(board: MutableList<MutableList<Boolean>>, row: Int, col: Int): Int {
    var size: Int = board.size
    var alive: Int = 0
    var dr: Int = 0 - 1
    while (dr < 2) {
        var dc: Int = 0 - 1
        while (dc < 2) {
            var nr: Int = row + dr
            var nc: Int = col + dc
            if ((((((((!(((dr == 0) && (dc == 0)) as Boolean) as Boolean) && (nr >= 0) as Boolean)) && (nr < size) as Boolean)) && (nc >= 0) as Boolean)) && (nc < size)) {
                if ((((((board[nr]!!) as MutableList<Boolean>))[nc]!!) as Boolean)) {
                    alive = alive + 1
                }
            }
            dc = dc + 1
        }
        dr = dr + 1
    }
    return alive
}

fun next_state(current: Boolean, alive: Int): Boolean {
    var state: Boolean = current
    if ((current as Boolean)) {
        if (alive < 2) {
            state = false
        } else {
            if ((alive == 2) || (alive == 3)) {
                state = true
            } else {
                state = false
            }
        }
    } else {
        if (alive == 3) {
            state = true
        }
    }
    return state
}

fun step(board: MutableList<MutableList<Boolean>>): MutableList<MutableList<Boolean>> {
    var size: Int = board.size
    var new_board: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var r: Int = 0
    while (r < size) {
        var new_row: MutableList<Boolean> = mutableListOf<Boolean>()
        var c: Int = 0
        while (c < size) {
            var alive: Int = count_alive_neighbours(board, r, c)
            var cell: Boolean = (((board[r]!!) as MutableList<Boolean>))[c]!!
            var updated: Boolean = next_state(cell, alive)
            new_row = run { val _tmp = new_row.toMutableList(); _tmp.add(updated); _tmp }
            c = c + 1
        }
        new_board = run { val _tmp = new_board.toMutableList(); _tmp.add(new_row); _tmp }
        r = r + 1
    }
    return new_board
}

fun show(board: MutableList<MutableList<Boolean>>): Unit {
    var r: Int = 0
    while (r < board.size) {
        var line: String = ""
        var c: Int = 0
        while (c < (board[r]!!).size) {
            if ((((((board[r]!!) as MutableList<Boolean>))[c]!!) as Boolean)) {
                line = line + "#"
            } else {
                line = line + "."
            }
            c = c + 1
        }
        println(line)
        r = r + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Initial")
        show(board)
        var i: Int = 0
        while (i < 4) {
            board = step(board)
            println("\nStep " + (i + 1).toString())
            show(board)
            i = i + 1
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
