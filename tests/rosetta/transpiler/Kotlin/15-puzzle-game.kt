var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        _nowSeed.toInt()
    } else {
        System.nanoTime().toInt()
    }
}

fun input(): String = readLine() ?: ""

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class MoveResult(var idx: Int, var ok: Boolean)
var board: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)
val solved: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)
var empty: Int = 15
var moves: Int = 0
var quit: Boolean = false
fun randMove(): Int {
    return _now() % 4
}

fun isSolved(): Boolean {
    var i: Int = 0
    while (i < 16) {
        if (board[i] != solved[i]) {
            return false
        }
        i = i + 1
    }
    return true
}

fun isValidMove(m: Int): MoveResult {
    if (m == 0) {
        return MoveResult(idx = empty - 4, ok = (empty / 4) > 0)
    }
    if (m == 1) {
        return MoveResult(idx = empty + 4, ok = (empty / 4) < 3)
    }
    if (m == 2) {
        return MoveResult(idx = empty + 1, ok = (empty % 4) < 3)
    }
    if (m == 3) {
        return MoveResult(idx = empty - 1, ok = (empty % 4) > 0)
    }
    return MoveResult(idx = 0, ok = false)
}

fun doMove(m: Int): Boolean {
    val r: MoveResult = isValidMove(m)
    if (!(r.ok as Boolean)) {
        return false
    }
    val i: Int = empty
    val j = r.idx
    val tmp: Int = board[i]
    board[i] = (board)[j] as Int
    board[j] = tmp
    empty = j as Int
    moves = moves + 1
    return true
}

fun shuffle(n: Int): Unit {
    var i: Int = 0
    while ((i < n) || isSolved()) {
        if ((doMove(randMove())) as Boolean) {
            i = i + 1
        }
    }
}

fun printBoard(): Unit {
    var line: String = ""
    var i: Int = 0
    while (i < 16) {
        val _val: Int = board[i]
        if (_val == 0) {
            line = line + "  ."
        } else {
            val s: String = _val.toString()
            if (_val < 10) {
                line = (line + "  ") + s
            } else {
                line = (line + " ") + s
            }
        }
        if ((i % 4) == 3) {
            println(line)
            line = ""
        }
        i = i + 1
    }
}

fun playOneMove(): Unit {
    while (true) {
        println(("Enter move #" + (moves + 1).toString()) + " (U, D, L, R, or Q): ")
        val s: String = input()
        if (s == "") {
            continue
        }
        val c: String = s.substring(0, 1)
        var m: Int = 0
        if ((c == "U") || (c == "u")) {
            m = 0
        } else {
            if ((c == "D") || (c == "d")) {
                m = 1
            } else {
                if ((c == "R") || (c == "r")) {
                    m = 2
                } else {
                    if ((c == "L") || (c == "l")) {
                        m = 3
                    } else {
                        if ((c == "Q") || (c == "q")) {
                            println(("Quiting after " + moves.toString()) + " moves.")
                            quit = true
                            return
                        } else {
                            println((("Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" + "up, down, left, or right. You can also enter \"Q\" to quit.\n") + "Upper or lowercase is accepted and only the first non-blank\n") + "character is important (i.e. you may enter \"up\" if you like).")
                            continue
                        }
                    }
                }
            }
        }
        if (!doMove(m)) {
            println("That is not a valid move at the moment.")
            continue
        }
        return
    }
}

fun play(): Unit {
    println("Starting board:")
    while ((!quit as Boolean) && (isSolved() == false)) {
        println("")
        printBoard()
        playOneMove()
    }
    if ((isSolved()) as Boolean) {
        println(("You solved the puzzle in " + moves.toString()) + " moves.")
    }
}

fun user_main(): Unit {
    shuffle(50)
    play()
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
