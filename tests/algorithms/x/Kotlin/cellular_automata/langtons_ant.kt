fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

fun create_board(width: Int, height: Int): MutableList<MutableList<Boolean>> {
    var board: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = 0
    while (i < height) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = 0
        while (j < width) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(true); _tmp }
            j = j + 1
        }
        board = run { val _tmp = board.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return board
}

fun move_ant(board: MutableList<MutableList<Boolean>>, x: Int, y: Int, direction: Int): MutableList<Int> {
    var direction: Int = direction
    var y: Int = y
    var x: Int = x
    if ((((((board[x]!!) as MutableList<Boolean>))[y]!!) as Boolean)) {
        direction = Math.floorMod((direction + 1), 4)
    } else {
        direction = Math.floorMod((direction + 3), 4)
    }
    var old_x: Int = x
    var old_y: Int = y
    if (direction == 0) {
        x = x - 1
    } else {
        if (direction == 1) {
            y = y + 1
        } else {
            if (direction == 2) {
                x = x + 1
            } else {
                y = y - 1
            }
        }
    }
    _listSet(board[old_x]!!, old_y, (!(((((board[old_x]!!) as MutableList<Boolean>))[old_y]!!) as? Boolean ?: false) as Boolean))
    return mutableListOf(x, y, direction)
}

fun langtons_ant(width: Int, height: Int, steps: Int): MutableList<MutableList<Boolean>> {
    var board: MutableList<MutableList<Boolean>> = create_board(width, height)
    var x: Int = width / 2
    var y: Int = height / 2
    var dir: Int = 3
    var s: Int = 0
    while (s < steps) {
        var state: MutableList<Int> = move_ant(board, x, y, dir)
        x = state[0]!!
        y = state[1]!!
        dir = state[2]!!
        s = s + 1
    }
    return board
}

fun test_first_move(): Unit {
    var board: MutableList<MutableList<Boolean>> = langtons_ant(2, 2, 1)
    expect(board == mutableListOf(mutableListOf(true, true), mutableListOf(true, false)))
}

fun test_second_move(): Unit {
    var board: MutableList<MutableList<Boolean>> = langtons_ant(2, 2, 2)
    expect(board == mutableListOf(mutableListOf(true, false), mutableListOf(true, false)))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_first_move()
        test_second_move()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
