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

fun randInt(seed: Int, n: Int): MutableList<Int> {
    var next: BigInteger = (Math.floorMod(((seed * 1664525) + 1013904223), 2147483647)).toBigInteger()
    return mutableListOf<Int>(next.toInt(), (next.remainder(n.toBigInteger())).toInt())
}

fun newBoard(n: Int, seed: Int): MutableList<Any?> {
    var board: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var s: Int = seed
    var i: Int = 0
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            var r: MutableList<Int> = randInt(s, 2)
            s = r[0]!!
            row = run { val _tmp = row.toMutableList(); _tmp.add(r[1]!!); _tmp } as MutableList<Int>
            j = j + 1
        }
        board = run { val _tmp = board.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        i = i + 1
    }
    return mutableListOf<Any?>(board as Any?, s as Any?)
}

fun copyBoard(b: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var nb: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < b.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < (b[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((b[i]!!) as MutableList<Int>)[j]!!); _tmp } as MutableList<Int>
            j = j + 1
        }
        nb = run { val _tmp = nb.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        i = i + 1
    }
    return nb
}

fun flipRow(b: MutableList<MutableList<Int>>, r: Int): MutableList<MutableList<Int>> {
    var j: Int = 0
    while (j < (b[r]!!).size) {
        ((b[r]!!)[j]) = 1 - ((b[r]!!) as MutableList<Int>)[j]!!
        j = j + 1
    }
    return b
}

fun flipCol(b: MutableList<MutableList<Int>>, c: Int): MutableList<MutableList<Int>> {
    var i: Int = 0
    while (i < b.size) {
        ((b[i]!!)[c]) = 1 - ((b[i]!!) as MutableList<Int>)[c]!!
        i = i + 1
    }
    return b
}

fun boardsEqual(a: MutableList<MutableList<Int>>, b: MutableList<MutableList<Int>>): Boolean {
    var i: Int = 0
    while (i < a.size) {
        var j: Int = 0
        while (j < (a[i]!!).size) {
            if (((a[i]!!) as MutableList<Int>)[j]!! != ((b[i]!!) as MutableList<Int>)[j]!!) {
                return false
            }
            j = j + 1
        }
        i = i + 1
    }
    return true
}

fun shuffleBoard(b: MutableList<MutableList<Int>>, seed: Int): MutableList<Any?> {
    var b: MutableList<MutableList<Int>> = b
    var s: Int = seed
    var n: Int = b.size
    var k: Int = 0
    while (k < (2 * n)) {
        var r: MutableList<Int> = randInt(s, n)
        s = r[0]!!
        var idx: Int = (r[1]!!).toInt()
        if ((Math.floorMod(k, 2)) == 0) {
            b = flipRow(b, idx)
        } else {
            b = flipCol(b, idx)
        }
        k = k + 1
    }
    return mutableListOf<Any?>(b as Any?, s as Any?)
}

fun solve(board: MutableList<MutableList<Int>>, target: MutableList<MutableList<Int>>): MutableMap<String, MutableList<Int>> {
    var n: Int = board.size
    var row: MutableList<Int> = mutableListOf<Int>()
    var col: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < n) {
        var diff: Int = if (((board[i]!!) as MutableList<Int>)[0]!! != ((target[i]!!) as MutableList<Int>)[0]!!) 1 else 0
        row = run { val _tmp = row.toMutableList(); _tmp.add(diff); _tmp } as MutableList<Int>
        i = i + 1
    }
    var j: Int = 0
    while (j < n) {
        var diff: Int = if (((board[0]!!) as MutableList<Int>)[j]!! != ((target[0]!!) as MutableList<Int>)[j]!!) 1 else 0
        var _val: BigInteger = (Math.floorMod((diff + row[0]!!), 2)).toBigInteger()
        col = run { val _tmp = col.toMutableList(); _tmp.add(_val.toInt()); _tmp } as MutableList<Int>
        j = j + 1
    }
    return mutableMapOf<String, MutableList<Int>>("row" to (row), "col" to (col))
}

fun applySolution(b: MutableList<MutableList<Int>>, sol: MutableMap<String, MutableList<Int>>): MutableList<Any?> {
    var board: MutableList<MutableList<Int>> = b
    var moves: Int = 0
    var i: Int = 0
    while (i < ((sol)["row"] as MutableList<Int>).size) {
        if ((((sol)["row"] as MutableList<Int>) as MutableList<Int>)[i]!! == 1) {
            board = flipRow(board, i)
            moves = moves + 1
        }
        i = i + 1
    }
    var j: Int = 0
    while (j < ((sol)["col"] as MutableList<Int>).size) {
        if ((((sol)["col"] as MutableList<Int>) as MutableList<Int>)[j]!! == 1) {
            board = flipCol(board, j)
            moves = moves + 1
        }
        j = j + 1
    }
    return mutableListOf<Any?>(board as Any?, moves as Any?)
}

fun printBoard(b: MutableList<MutableList<Int>>): Unit {
    var i: Int = 0
    while (i < b.size) {
        var line: String = ""
        var j: Int = 0
        while (j < (b[i]!!).size) {
            line = line + (((b[i]!!) as MutableList<Int>)[j]!!).toString()
            if (j < ((b[i]!!).size - 1)) {
                line = line + " "
            }
            j = j + 1
        }
        println(line)
        i = i + 1
    }
}

fun user_main(): Unit {
    var n: Int = 3
    var seed: Int = 1
    var res: MutableList<Any?> = newBoard(n, seed)
    var target: MutableList<MutableList<Int>> = (res[0] as Any?) as MutableList<MutableList<Int>>
    seed = res[1] as Int
    var board: MutableList<MutableList<Int>> = copyBoard(target)
    while (true) {
        var sres: MutableList<Any?> = shuffleBoard(copyBoard(board), seed)
        board = (sres[0] as Any?) as MutableList<MutableList<Int>>
        seed = sres[1] as Int
        if (!boardsEqual(board, target)) {
            break
        }
    }
    println("Target:")
    printBoard(target)
    println("Board:")
    printBoard(board)
    var sol: MutableMap<String, MutableList<Int>> = solve(board, target)
    var ares: MutableList<Any?> = applySolution(board, sol)
    board = (ares[0] as Any?) as MutableList<MutableList<Int>>
    var moves: Int = ares[1] as Int
    println("Solved:")
    printBoard(board)
    println("Moves: " + moves.toString())
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
