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

fun randMove(): Int {
    return _now() % 4
}

fun isSolved(): Boolean {
    var i: Int = 0
    while (i < 16) {
        if ((board[i] as Int) != (solved[i] as Int)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun isValidMove(m: Int): MutableMap<String, any> {
    if (m == 0) {
        return mutableMapOf<String, Any>("idx" to empty - 4, "ok" to (empty / 4) > 0)
    }
    if (m == 1) {
        return mutableMapOf<String, Any>("idx" to empty + 4, "ok" to (empty / 4) < 3)
    }
    if (m == 2) {
        return mutableMapOf<String, Any>("idx" to empty + 1, "ok" to (empty % 4) < 3)
    }
    if (m == 3) {
        return mutableMapOf<String, Any>("idx" to empty - 1, "ok" to (empty % 4) > 0)
    }
    return mutableMapOf<String, Any>("idx" to 0, "ok" to false)
}

fun doMove(m: Int): Boolean {
    val r = isValidMove(m)
    if (!((r["ok"]!!) as Boolean)) {
        return false
    }
    val i: Int = empty
    val j = int((r["idx"]!!))
    val tmp: Int = (board[i] as Int)
    board[i] = (board[j] as Int)
    board[j] = tmp
    empty = j
    moves = moves + 1
    return true
}

fun shuffle(n: Int): Unit {
    var i: Int = 0
    while ((i < n) || isSolved()) {
        if (doMove(randMove()) as Boolean) {
            i = i + 1
        }
    }
}

fun printBoard(): Unit {
    var line: String = ""
    var i: Int = 0
    while (i < 16) {
        val _val: Int = (board[i] as Int)
        if (_val == 0) {
            line = line + "  ."
        } else {
            val s = _val.toString()
            if (_val < 10) {
                line = (line + "  ") + (s).toString()
            } else {
                line = (line + " ") + (s).toString()
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
        println(("Enter move #" + (moves + 1.toString()).toString()) + " (U, D, L, R, or Q): ")
        val s = input()
        if (s == "") {
            continue
        }
        val c = s.subList(0, 1)
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
                            println(("Quiting after " + (moves.toString()).toString()) + " moves.")
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
        if (!(doMove(m) as Boolean)) {
            println("That is not a valid move at the moment.")
            continue
        }
        return
    }
}

fun play(): Unit {
    println("Starting board:")
    while (!quit && (isSolved() == false)) {
        println("")
        printBoard()
        playOneMove()
    }
    if (isSolved() as Boolean) {
        println(("You solved the puzzle in " + (moves.toString()).toString()) + " moves.")
    }
}

fun user_main(): Unit {
    shuffle(50)
    play()
}

fun main() {
    var board: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)
    val solved: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0)
    var empty: Int = 15
    var moves: Int = 0
    var quit: Boolean = false
    user_main()
}
