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

data class Board(var cells: MutableList<MutableList<Int>>)
data class SpawnResult(var board: Board, var full: Boolean)
data class SlideResult(var row: MutableList<Int>, var gain: Int)
data class MoveResult(var board: Board, var score: Int, var moved: Boolean)
val SIZE: Int = 4
var board: Board = newBoard()
var r: SpawnResult = spawnTile(board)
var full: Boolean = r.full
var score: Int = 0
fun newBoard(): Board {
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
    return Board(cells = b)
}

fun spawnTile(b: Board): SpawnResult {
    var grid: MutableList<MutableList<Int>> = b.cells
    var empty: MutableList<MutableList<Int>> = mutableListOf()
    var y: Int = 0
    while (y < SIZE) {
        var x: Int = 0
        while (x < SIZE) {
            if (grid[y][x] == 0) {
                empty = run { val _tmp = empty.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp } as MutableList<MutableList<Int>>
            }
            x = x + 1
        }
        y = y + 1
    }
    if (empty.size == 0) {
        return SpawnResult(board = b, full = true)
    }
    var idx: Int = _now() % empty.size
    val cell: MutableList<Int> = empty[idx]
    var _val: Int = 4
    if ((_now() % 10) < 9) {
        _val = 2
    }
    grid[cell[1]][cell[0]] = _val
    return SpawnResult(board = Board(cells = grid), full = empty.size == 1)
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

fun draw(b: Board, score: Int): Unit {
    println("Score: " + score.toString())
    var y: Int = 0
    while (y < SIZE) {
        println("+----+----+----+----+")
        var line: String = "|"
        var x: Int = 0
        while (x < SIZE) {
            var v: Int = b.cells[y][x]
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

fun slideLeft(row: MutableList<Int>): SlideResult {
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
    return SlideResult(row = res, gain = gain)
}

fun moveLeft(b: Board, score: Int): MoveResult {
    var score: Int = score
    var grid: MutableList<MutableList<Int>> = b.cells
    var moved: Boolean = false
    var y: Int = 0
    while (y < SIZE) {
        val r: SlideResult = slideLeft(grid[y] as MutableList<Int>)
        val new = r.row
        score = score + (r.gain as Int)
        var x: Int = 0
        while (x < SIZE) {
            if (grid[y][x] != new[x]) {
                moved = true
            }
            grid[y][x] = new[x]
            x = x + 1
        }
        y = y + 1
    }
    return MoveResult(board = Board(cells = grid), score = score, moved = moved)
}

fun moveRight(b: Board, score: Int): MoveResult {
    var score: Int = score
    var grid: MutableList<MutableList<Int>> = b.cells
    var moved: Boolean = false
    var y: Int = 0
    while (y < SIZE) {
        var rev: MutableList<Int> = reverseRow(grid[y] as MutableList<Int>)
        val r: SlideResult = slideLeft(rev)
        rev = r.row as MutableList<Int>
        score = score + (r.gain as Int)
        rev = reverseRow(rev) as MutableList<Int>
        var x: Int = 0
        while (x < SIZE) {
            if (grid[y][x] != rev[x]) {
                moved = true
            }
            grid[y][x] = rev[x]
            x = x + 1
        }
        y = y + 1
    }
    return MoveResult(board = Board(cells = grid), score = score, moved = moved)
}

fun getCol(b: Board, x: Int): MutableList<Int> {
    var col: MutableList<Int> = mutableListOf()
    var y: Int = 0
    while (y < SIZE) {
        col = run { val _tmp = col.toMutableList(); _tmp.add(b.cells[y][x]); _tmp } as MutableList<Int>
        y = y + 1
    }
    return col
}

fun setCol(b: Board, x: Int, col: MutableList<Int>): Unit {
    var rows: MutableList<MutableList<Int>> = b.cells
    var y: Int = 0
    while (y < SIZE) {
        var row = rows[y]
        row[x] = col[y]
        rows[y] = row
        y = y + 1
    }
    b.cells = rows
}

fun moveUp(b: Board, score: Int): MoveResult {
    var score: Int = score
    var grid: MutableList<MutableList<Int>> = b.cells
    var moved: Boolean = false
    var x: Int = 0
    while (x < SIZE) {
        var col: MutableList<Int> = getCol(b, x)
        val r: SlideResult = slideLeft(col)
        val new = r.row
        score = score + (r.gain as Int)
        var y: Int = 0
        while (y < SIZE) {
            if (grid[y][x] != new[y]) {
                moved = true
            }
            grid[y][x] = new[y]
            y = y + 1
        }
        x = x + 1
    }
    return MoveResult(board = Board(cells = grid), score = score, moved = moved)
}

fun moveDown(b: Board, score: Int): MoveResult {
    var score: Int = score
    var grid: MutableList<MutableList<Int>> = b.cells
    var moved: Boolean = false
    var x: Int = 0
    while (x < SIZE) {
        var col: MutableList<Int> = reverseRow(getCol(b, x) as MutableList<Int>)
        val r: SlideResult = slideLeft(col)
        col = r.row as MutableList<Int>
        score = score + (r.gain as Int)
        col = reverseRow(col) as MutableList<Int>
        var y: Int = 0
        while (y < SIZE) {
            if (grid[y][x] != col[y]) {
                moved = true
            }
            grid[y][x] = col[y]
            y = y + 1
        }
        x = x + 1
    }
    return MoveResult(board = Board(cells = grid), score = score, moved = moved)
}

fun hasMoves(b: Board): Boolean {
    var y: Int = 0
    while (y < SIZE) {
        var x: Int = 0
        while (x < SIZE) {
            if (b.cells[y][x] == 0) {
                return true
            }
            if (((x + 1) < SIZE) && (b.cells[y][x] == b.cells[y][x + 1])) {
                return true
            }
            if (((y + 1) < SIZE) && (b.cells[y][x] == b.cells[y + 1][x])) {
                return true
            }
            x = x + 1
        }
        y = y + 1
    }
    return false
}

fun has2048(b: Board): Boolean {
    var y: Int = 0
    while (y < SIZE) {
        var x: Int = 0
        while (x < SIZE) {
            if (b.cells[y][x] >= 2048) {
                return true
            }
            x = x + 1
        }
        y = y + 1
    }
    return false
}

fun main() {
    board = r.board
    r = spawnTile(board)
    board = r.board
    full = r.full
    draw(board, score)
    while (true) {
        println("Move: ")
        val cmd: String = input()
        var moved: Boolean = false
        if ((cmd == "a") || (cmd == "A")) {
            val m: MoveResult = moveLeft(board, score)
            board = m.board as Board
            score = m.score as Int
            moved = m.moved as Boolean
        }
        if ((cmd == "d") || (cmd == "D")) {
            val m: MoveResult = moveRight(board, score)
            board = m.board as Board
            score = m.score as Int
            moved = m.moved as Boolean
        }
        if ((cmd == "w") || (cmd == "W")) {
            val m: MoveResult = moveUp(board, score)
            board = m.board as Board
            score = m.score as Int
            moved = m.moved as Boolean
        }
        if ((cmd == "s") || (cmd == "S")) {
            val m: MoveResult = moveDown(board, score)
            board = m.board as Board
            score = m.score as Int
            moved = m.moved as Boolean
        }
        if ((cmd == "q") || (cmd == "Q")) {
            break
        }
        if (moved as Boolean) {
            val r2: SpawnResult = spawnTile(board)
            board = r2.board as Board
            full = r2.full as Boolean
            if (full && (!(hasMoves(board) as Boolean) as Boolean)) {
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
