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

val SIZE: Int = 4
var board: MutableList<MutableList<Int>> = newBoard()
var r: MutableMap<String, Any> = spawnTile(board)
var full = (r)["full"]!!
var score: Int = 0
fun newBoard(): MutableList<MutableList<Int>> {
    var b: MutableList<MutableList<Int>> = mutableListOf()
    var y: Int = 0
    while (y < SIZE) {
        var row: MutableList<Int> = mutableListOf()
        var x: Int = 0
        while (x < SIZE) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            x = x + 1
        }
        b = run { val _tmp = b.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        y = y + 1
    }
    return b
}

fun spawnTile(b: MutableList<MutableList<Int>>): MutableMap<String, Any> {
    var empty: MutableList<MutableList<Int>> = mutableListOf()
    var y: Int = 0
    while (y < SIZE) {
        var x: Int = 0
        while (x < SIZE) {
            if (b[y][x] == 0) {
                empty = run { val _tmp = empty.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp } as MutableList<MutableList<Int>>
            }
            x = x + 1
        }
        y = y + 1
    }
    if (empty.size == 0) {
        return mutableMapOf<String, Any>("board" to (b), "full" to (true))
    }
    var idx: Int = _now() % empty.size
    val cell: MutableList<Int> = empty[idx]
    var _val: Int = 4
    if ((_now() % 10) < 9) {
        _val = 2
    }
    b[cell[1]][cell[0]] = _val
    return mutableMapOf<String, Any>("board" to (b), "full" to (empty.size == 1))
}

fun pad(n: Int): String {
    var s: String = n.toString()
    var pad: Int = 4 - s.length
    var i: Int = 0
    var out: String = ""
    while (i < pad) {
        out = out + " "
        i = i + 1
    }
    return out + s
}

fun draw(b: MutableList<MutableList<Int>>, score: Int): Unit {
    println("Score: " + score.toString())
    var y: Int = 0
    while (y < SIZE) {
        println("+----+----+----+----+")
        var line: String = "|"
        var x: Int = 0
        while (x < SIZE) {
            var v: Int = b[y][x]
            if (v == 0) {
                line = line + "    |"
            } else {
                line = (line + (pad(v)).toString()) + "|"
            }
            x = x + 1
        }
        println(line)
        y = y + 1
    }
    println("+----+----+----+----+")
    println("W=Up S=Down A=Left D=Right Q=Quit")
}

fun reverseRow(r: MutableList<Int>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf()
    var i: Int = r.size - 1
    while (i >= 0) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(r[i]); _tmp } as MutableList<Int>
        i = i - 1
    }
    return out
}

fun slideLeft(row: MutableList<Int>): MutableMap<String, Any> {
    var xs: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < row.size) {
        if (row[i] != 0) {
            xs = run { val _tmp = xs.toMutableList(); _tmp.add(row[i]); _tmp } as MutableList<Int>
        }
        i = i + 1
    }
    var res: MutableList<Int> = mutableListOf()
    var gain: Int = 0
    i = 0
    while (i < xs.size) {
        if (((i + 1) < xs.size) && (xs[i] == xs[i + 1])) {
            val v: Int = xs[i] * 2
            gain = gain + v
            res = run { val _tmp = res.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
            i = i + 2
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]); _tmp } as MutableList<Int>
            i = i + 1
        }
    }
    while (res.size < SIZE) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
    }
    return mutableMapOf<String, Any>("row" to (res), "gain" to (gain))
}

fun moveLeft(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var score: Int = score
    var moved: Boolean = false
    var y: Int = 0
    while (y < SIZE) {
        val r: MutableMap<String, Any> = slideLeft(b[y])
        val new = (r)["row"]!!
        score = score + ((r)["gain"]!! as Int) as Int
        var x: Int = 0
        while (x < SIZE) {
            if (b[y][x] != (new as MutableList<Any?>)[x]!!) {
                moved = true
            }
            b[y][x] = (new as MutableList<Any?>)[x]!! as Int
            x = x + 1
        }
        y = y + 1
    }
    return mutableMapOf<String, Any>("board" to (b), "score" to (score), "moved" to (moved))
}

fun moveRight(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var score: Int = score
    var moved: Boolean = false
    var y: Int = 0
    while (y < SIZE) {
        var rev: MutableList<Int> = reverseRow(b[y])
        val r: MutableMap<String, Any> = slideLeft(rev)
        rev = (r)["row"]!! as MutableList<Int>
        score = score + ((r)["gain"]!! as Int) as Int
        rev = reverseRow(rev) as MutableList<Int>
        var x: Int = 0
        while (x < SIZE) {
            if (b[y][x] != rev[x]) {
                moved = true
            }
            b[y][x] = rev[x]
            x = x + 1
        }
        y = y + 1
    }
    return mutableMapOf<String, Any>("board" to (b), "score" to (score), "moved" to (moved))
}

fun getCol(b: MutableList<MutableList<Int>>, x: Int): MutableList<Int> {
    var col: MutableList<Int> = mutableListOf()
    var y: Int = 0
    while (y < SIZE) {
        col = run { val _tmp = col.toMutableList(); _tmp.add(b[y][x]); _tmp } as MutableList<Int>
        y = y + 1
    }
    return col
}

fun setCol(b: MutableList<MutableList<Int>>, x: Int, col: MutableList<Int>): Unit {
    var y: Int = 0
    while (y < SIZE) {
        b[y][x] = col[y]
        y = y + 1
    }
}

fun moveUp(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var score: Int = score
    var moved: Boolean = false
    var x: Int = 0
    while (x < SIZE) {
        var col: MutableList<Int> = getCol(b, x)
        val r: MutableMap<String, Any> = slideLeft(col)
        val new = (r)["row"]!!
        score = score + ((r)["gain"]!! as Int) as Int
        var y: Int = 0
        while (y < SIZE) {
            if (b[y][x] != (new as MutableList<Any?>)[y]!!) {
                moved = true
            }
            b[y][x] = (new as MutableList<Any?>)[y]!! as Int
            y = y + 1
        }
        x = x + 1
    }
    return mutableMapOf<String, Any>("board" to (b), "score" to (score), "moved" to (moved))
}

fun moveDown(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var score: Int = score
    var moved: Boolean = false
    var x: Int = 0
    while (x < SIZE) {
        var col: MutableList<Int> = reverseRow(getCol(b, x))
        val r: MutableMap<String, Any> = slideLeft(col)
        col = (r)["row"]!! as MutableList<Int>
        score = score + ((r)["gain"]!! as Int) as Int
        col = reverseRow(col) as MutableList<Int>
        var y: Int = 0
        while (y < SIZE) {
            if (b[y][x] != col[y]) {
                moved = true
            }
            b[y][x] = col[y]
            y = y + 1
        }
        x = x + 1
    }
    return mutableMapOf<String, Any>("board" to (b), "score" to (score), "moved" to (moved))
}

fun hasMoves(b: MutableList<MutableList<Int>>): Boolean {
    var y: Int = 0
    while (y < SIZE) {
        var x: Int = 0
        while (x < SIZE) {
            if (b[y][x] == 0) {
                return true
            }
            if (((x + 1) < SIZE) && (b[y][x] == b[y][x + 1])) {
                return true
            }
            if (((y + 1) < SIZE) && (b[y][x] == b[y + 1][x])) {
                return true
            }
            x = x + 1
        }
        y = y + 1
    }
    return false
}

fun has2048(b: MutableList<MutableList<Int>>): Boolean {
    var y: Int = 0
    while (y < SIZE) {
        var x: Int = 0
        while (x < SIZE) {
            if (b[y][x] >= 2048) {
                return true
            }
            x = x + 1
        }
        y = y + 1
    }
    return false
}

fun main() {
    board = (r)["board"]!! as MutableList<MutableList<Int>>
    r = spawnTile(board) as MutableMap<String, Any>
    board = (r)["board"]!! as MutableList<MutableList<Int>>
    full = (r)["full"]!!
    draw(board, score)
    while (true) {
        println("Move: ")
        val cmd: String = input()
        var moved: Boolean = false
        if ((cmd == "a") || (cmd == "A")) {
            val m: MutableMap<String, Any> = moveLeft(board, score)
            board = (m)["board"]!! as MutableList<MutableList<Int>>
            score = (m)["score"]!! as Int
            moved = (m)["moved"]!! as Boolean
        }
        if ((cmd == "d") || (cmd == "D")) {
            val m: MutableMap<String, Any> = moveRight(board, score)
            board = (m)["board"]!! as MutableList<MutableList<Int>>
            score = (m)["score"]!! as Int
            moved = (m)["moved"]!! as Boolean
        }
        if ((cmd == "w") || (cmd == "W")) {
            val m: MutableMap<String, Any> = moveUp(board, score)
            board = (m)["board"]!! as MutableList<MutableList<Int>>
            score = (m)["score"]!! as Int
            moved = (m)["moved"]!! as Boolean
        }
        if ((cmd == "s") || (cmd == "S")) {
            val m: MutableMap<String, Any> = moveDown(board, score)
            board = (m)["board"]!! as MutableList<MutableList<Int>>
            score = (m)["score"]!! as Int
            moved = (m)["moved"]!! as Boolean
        }
        if ((cmd == "q") || (cmd == "Q")) {
            break
        }
        if (moved as Boolean) {
            val r2: MutableMap<String, Any> = spawnTile(board)
            board = (r2)["board"]!! as MutableList<MutableList<Int>>
            full = (r2)["full"]!!
            if ((full as Boolean) && (!(hasMoves(board) as Boolean) as Boolean)) {
                draw(board, score)
                println("Game Over")
                break
            }
        }
        draw(board, score)
        if (has2048(board) as Boolean) {
            println("You win!")
            break
        }
        if (!(hasMoves(board) as Boolean)) {
            println("Game Over")
            break
        }
    }
}
