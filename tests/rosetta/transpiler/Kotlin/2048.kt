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
var board = newBoard()
var r: MutableMap<String, Any> = spawnTile(board)
var full = (r["full"]!!)
var score: Int = 0
fun newBoard(): MutableList<MutableList<Int>> {
    var b: MutableList<MutableList<Int>> = mutableListOf()
    var y: Int = 0
    while (y < SIZE) {
        var row: MutableList<Int> = mutableListOf()
        var x: Int = 0
        while (x < SIZE) {
            row = (row + 0).toMutableList()
            x = x + 1
        }
        b = (b + row).toMutableList()
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
            if (((b[y] as MutableList<Int>)[x]!!) == 0) {
                empty = (empty + mutableListOf(x, y)).toMutableList()
            }
            x = x + 1
        }
        y = y + 1
    }
    if (empty.size == 0) {
        return mutableMapOf<String, Any>("board" to (b), "full" to (true))
    }
    var idx: Double = _now() % (empty.size as Number).toDouble()
    val cell: MutableList<Int> = (empty[idx] as MutableList<Int>)
    var _val: Int = 4
    if ((_now() % 10) < 9) {
        _val = 2
    }
    (b[(cell[1] as Int)] as MutableList<Int>)[(cell[0] as Int)] = _val
    return mutableMapOf<String, Any>("board" to (b), "full" to (empty.size == 1))
}

fun pad(n: Int): String {
    var s: String = n.toString()
    var pad: (Int) -> String = 4 - (s.length as Number).toDouble()
    var i: Int = 0
    var out: String = ""
    while (i < (pad as Number).toDouble()) {
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
            var v: Any = ((b[y] as MutableList<Int>)[x]!!)
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
    var i: Double = (r.size as Number).toDouble() - 1
    while (i >= 0) {
        out = (out + (r[i] as Int)).toMutableList()
        i = i - 1
    }
    return out
}

fun slideLeft(row: MutableList<Int>): MutableMap<String, Any> {
    var xs: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < (row.size as Number).toDouble()) {
        if ((row[i] as Int) != 0) {
            xs = (xs + (row[i] as Int)).toMutableList()
        }
        i = i + 1
    }
    var res: MutableList<Int> = mutableListOf()
    var gain: Int = 0
    i = 0
    while (i < (xs.size as Number).toDouble()) {
        if (((i + 1) < (xs.size as Number).toDouble()) && ((xs[i] as Int) == (xs[i + 1] as Int))) {
            val v: Int = (xs[i] as Int) * 2
            gain = gain + v
            res = (res + v).toMutableList()
            i = i + 2
        } else {
            res = (res + (xs[i] as Int)).toMutableList()
            i = i + 1
        }
    }
    while ((res.size as Number).toDouble() < SIZE) {
        res = (res + 0).toMutableList()
    }
    return mutableMapOf<String, Any>("row" to (res), "gain" to (gain))
}

fun moveLeft(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var moved: Boolean = false
    var y: Int = 0
    while (y < SIZE) {
        val r: MutableMap<String, Any> = slideLeft((b[y] as MutableList<Int>))
        val new: Any = (r["row"]!!)
        score = score + ((r["gain"]!!) as Number).toDouble()
        var x: Int = 0
        while (x < SIZE) {
            if (((b[y] as MutableList<Int>)[x]!!) != (new[x]!!)) {
                moved = true
            }
            (b[y] as MutableList<Int>)[x] = (new[x]!!)
            x = x + 1
        }
        y = y + 1
    }
    return mutableMapOf<String, Any>("board" to (b), "score" to (score), "moved" to (moved))
}

fun moveRight(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var moved: Boolean = false
    var y: Int = 0
    while (y < SIZE) {
        var rev = reverseRow((b[y] as MutableList<Int>))
        val r: MutableMap<String, Any> = slideLeft(rev)
        rev = (r["row"]!!)
        score = score + ((r["gain"]!!) as Number).toDouble()
        rev = reverseRow(rev)
        var x: Int = 0
        while (x < SIZE) {
            if (((b[y] as MutableList<Int>)[x]!!) != (rev[x]!!)) {
                moved = true
            }
            (b[y] as MutableList<Int>)[x] = (rev[x]!!)
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
        col = (col + ((b[y] as MutableList<Int>)[x]!!)).toMutableList()
        y = y + 1
    }
    return col
}

fun setCol(b: MutableList<MutableList<Int>>, x: Int, col: MutableList<Int>): Unit {
    var y: Int = 0
    while (y < SIZE) {
        (b[y] as MutableList<Int>)[x] = (col[y] as Int)
        y = y + 1
    }
}

fun moveUp(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var moved: Boolean = false
    var x: Int = 0
    while (x < SIZE) {
        var col = getCol(b, x)
        val r: MutableMap<String, Any> = slideLeft(col)
        val new: Any = (r["row"]!!)
        score = score + ((r["gain"]!!) as Number).toDouble()
        var y: Int = 0
        while (y < SIZE) {
            if (((b[y] as MutableList<Int>)[x]!!) != (new[y]!!)) {
                moved = true
            }
            (b[y] as MutableList<Int>)[x] = (new[y]!!)
            y = y + 1
        }
        x = x + 1
    }
    return mutableMapOf<String, Any>("board" to (b), "score" to (score), "moved" to (moved))
}

fun moveDown(b: MutableList<MutableList<Int>>, score: Int): MutableMap<String, Any> {
    var moved: Boolean = false
    var x: Int = 0
    while (x < SIZE) {
        var col = reverseRow(getCol(b, x))
        val r: MutableMap<String, Any> = slideLeft(col)
        col = (r["row"]!!)
        score = score + ((r["gain"]!!) as Number).toDouble()
        col = reverseRow(col)
        var y: Int = 0
        while (y < SIZE) {
            if (((b[y] as MutableList<Int>)[x]!!) != (col[y]!!)) {
                moved = true
            }
            (b[y] as MutableList<Int>)[x] = (col[y]!!)
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
            if (((b[y] as MutableList<Int>)[x]!!) == 0) {
                return true
            }
            if (((x + 1) < SIZE) && (((b[y] as MutableList<Int>)[x]!!) == ((b[y] as MutableList<Int>)[x + 1]!!))) {
                return true
            }
            if (((y + 1) < SIZE) && (((b[y] as MutableList<Int>)[x]!!) == ((b[y + 1] as MutableList<Int>)[x]!!))) {
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
            if ((((b[y] as MutableList<Int>)[x]!!) as Number).toDouble() >= 2048) {
                return true
            }
            x = x + 1
        }
        y = y + 1
    }
    return false
}

fun main() {
    board = (r["board"]!!)
    r = spawnTile(board)
    board = (r["board"]!!)
    full = (r["full"]!!)
    draw(board, score)
    while (true) {
        println("Move: ")
        val cmd: String = input()
        var moved: Boolean = false
        if ((cmd == "a") || (cmd == "A")) {
            val m = moveLeft(board, score)
            board = (m["board"]!!)
            score = (m["score"]!!)
            moved = (m["moved"]!!)
        }
        if ((cmd == "d") || (cmd == "D")) {
            val m = moveRight(board, score)
            board = (m["board"]!!)
            score = (m["score"]!!)
            moved = (m["moved"]!!)
        }
        if ((cmd == "w") || (cmd == "W")) {
            val m = moveUp(board, score)
            board = (m["board"]!!)
            score = (m["score"]!!)
            moved = (m["moved"]!!)
        }
        if ((cmd == "s") || (cmd == "S")) {
            val m = moveDown(board, score)
            board = (m["board"]!!)
            score = (m["score"]!!)
            moved = (m["moved"]!!)
        }
        if ((cmd == "q") || (cmd == "Q")) {
            break
        }
        if (moved as Boolean) {
            val r2 = spawnTile(board)
            board = (r2["board"]!!)
            full = (r2["full"]!!)
            if (full && !(hasMoves(board) as Boolean)) {
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
