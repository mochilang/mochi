import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class Coord(var x: Int = 0, var y: Int = 0)
data class PlayResult(var matrix: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>(), var score: Int = 0)
fun is_alnum(ch: String): Boolean {
    return ((((((ch >= "0") && (ch <= "9") as Boolean)) || (((ch >= "A") && (ch <= "Z") as Boolean)) as Boolean)) || (((ch >= "a") && (ch <= "z") as Boolean))) as Boolean
}

fun to_int(token: String): Int {
    var res: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < token.length) {
        res = (res * 10) + (token.substring(i, i + 1).toBigInteger().toInt())
        i = i + 1
    }
    return res
}

fun split(s: String, sep: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == sep) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + ch
        }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
    return res
}

fun parse_moves(input_str: String): MutableList<Coord> {
    var pairs: MutableList<String> = split(input_str, ",")
    var moves: MutableList<Coord> = mutableListOf<Coord>()
    var i: Int = (0).toInt()
    while (i < pairs.size) {
        var pair: String = pairs[i]!!
        var numbers: MutableList<String> = mutableListOf<String>()
        var num: String = ""
        var j: Int = (0).toInt()
        while (j < pair.length) {
            var ch: String = pair.substring(j, j + 1)
            if (ch == " ") {
                if (num != "") {
                    numbers = run { val _tmp = numbers.toMutableList(); _tmp.add(num); _tmp }
                    num = ""
                }
            } else {
                num = num + ch
            }
            j = j + 1
        }
        if (num != "") {
            numbers = run { val _tmp = numbers.toMutableList(); _tmp.add(num); _tmp }
        }
        if (numbers.size != 2) {
            panic("Each move must have exactly two numbers.")
        }
        var x: Int = (to_int(numbers[0]!!)).toInt()
        var y: Int = (to_int(numbers[1]!!)).toInt()
        moves = run { val _tmp = moves.toMutableList(); _tmp.add(Coord(x = x, y = y)); _tmp }
        i = i + 1
    }
    return moves
}

fun validate_matrix_size(size: Int): Unit {
    if (size <= 0) {
        panic("Matrix size must be a positive integer.")
    }
}

fun validate_matrix_content(matrix: MutableList<String>, size: Int): Unit {
    if (matrix.size != size) {
        panic("The matrix dont match with size.")
    }
    var i: Int = (0).toInt()
    while (i < size) {
        var row: String = matrix[i]!!
        if (row.length != size) {
            panic(("Each row in the matrix must have exactly " + _numToStr(size)) + " characters.")
        }
        var j: Int = (0).toInt()
        while (j < size) {
            var ch: String = row.substring(j, j + 1)
            if (!is_alnum(ch)) {
                panic("Matrix rows can only contain letters and numbers.")
            }
            j = j + 1
        }
        i = i + 1
    }
}

fun validate_moves(moves: MutableList<Coord>, size: Int): Unit {
    var i: Int = (0).toInt()
    while (i < moves.size) {
        var mv: Coord = moves[i]!!
        if ((((((mv.x < 0) || (mv.x >= size) as Boolean)) || (mv.y < 0) as Boolean)) || (mv.y >= size)) {
            panic("Move is out of bounds for a matrix.")
        }
        i = i + 1
    }
}

fun contains(pos: MutableList<Coord>, r: Int, c: Int): Boolean {
    var i: Int = (0).toInt()
    while (i < pos.size) {
        var p: Coord = pos[i]!!
        if ((p.x == r) && (p.y == c)) {
            return true
        }
        i = i + 1
    }
    return false
}

fun find_repeat(matrix_g: MutableList<MutableList<String>>, row: Int, column: Int, size: Int): MutableList<Coord> {
    var column: Int = (column).toInt()
    column = (size - 1) - column
    var visited: MutableList<Coord> = mutableListOf<Coord>()
    var repeated: MutableList<Coord> = mutableListOf<Coord>()
    var color: String = ((matrix_g[column]!!) as MutableList<String>)[row]!!
    if (color == "-") {
        return repeated
    }
    var stack: MutableList<Coord> = mutableListOf(Coord(x = column, y = row))
    while (stack.size > 0) {
        var idx: Int = (stack.size - 1).toInt()
        var pos: Coord = stack[idx]!!
        stack = _sliceList(stack, 0, idx)
        if ((((((pos.x < 0) || (pos.x >= size) as Boolean)) || (pos.y < 0) as Boolean)) || (pos.y >= size)) {
            continue
        }
        if ((contains(visited, pos.x, pos.y)) as Boolean) {
            continue
        }
        visited = run { val _tmp = visited.toMutableList(); _tmp.add(pos); _tmp }
        if (((matrix_g[pos.x]!!) as MutableList<String>)[pos.y]!! == color) {
            repeated = run { val _tmp = repeated.toMutableList(); _tmp.add(pos); _tmp }
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(Coord(x = pos.x - 1, y = pos.y)); _tmp }
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(Coord(x = pos.x + 1, y = pos.y)); _tmp }
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(Coord(x = pos.x, y = pos.y - 1)); _tmp }
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(Coord(x = pos.x, y = pos.y + 1)); _tmp }
        }
    }
    return repeated
}

fun increment_score(count: Int): Int {
    return (count * (count + 1)) / 2
}

fun move_x(matrix_g: MutableList<MutableList<String>>, column: Int, size: Int): MutableList<MutableList<String>> {
    var new_list: MutableList<String> = mutableListOf<String>()
    var row: Int = (0).toInt()
    while (row < size) {
        var _val: String = ((matrix_g[row]!!) as MutableList<String>)[column]!!
        if (_val != "-") {
            new_list = run { val _tmp = new_list.toMutableList(); _tmp.add(_val); _tmp }
        } else {
            new_list = concat(mutableListOf(_val), new_list)
        }
        row = row + 1
    }
    row = 0
    while (row < size) {
        _listSet(matrix_g[row]!!, column, new_list[row]!!)
        row = row + 1
    }
    return matrix_g
}

fun move_y(matrix_g: MutableList<MutableList<String>>, size: Int): MutableList<MutableList<String>> {
    var empty_cols: MutableList<Int> = mutableListOf<Int>()
    var column: Int = (size - 1).toInt()
    while (column >= 0) {
        var row: Int = (0).toInt()
        var all_empty: Boolean = true
        while (row < size) {
            if (((matrix_g[row]!!) as MutableList<String>)[column]!! != "-") {
                all_empty = false
                break
            }
            row = row + 1
        }
        if (all_empty as Boolean) {
            empty_cols = run { val _tmp = empty_cols.toMutableList(); _tmp.add(column); _tmp }
        }
        column = column - 1
    }
    var i: Int = (0).toInt()
    while (i < empty_cols.size) {
        var col: Int = (empty_cols[i]!!).toInt()
        var c: Int = (col + 1).toInt()
        while (c < size) {
            var r: Int = (0).toInt()
            while (r < size) {
                _listSet(matrix_g[r]!!, c - 1, ((matrix_g[r]!!) as MutableList<String>)[c]!!)
                r = r + 1
            }
            c = c + 1
        }
        var r: Int = (0).toInt()
        while (r < size) {
            _listSet(matrix_g[r]!!, size - 1, "-")
            r = r + 1
        }
        i = i + 1
    }
    return matrix_g
}

fun play(matrix_g: MutableList<MutableList<String>>, pos_x: Int, pos_y: Int, size: Int): PlayResult {
    var matrix_g: MutableList<MutableList<String>> = matrix_g
    var same_colors: MutableList<Coord> = find_repeat(matrix_g, pos_x, pos_y, size)
    if (same_colors.size != 0) {
        var i: Int = (0).toInt()
        while (i < same_colors.size) {
            var p: Coord = same_colors[i]!!
            _listSet(matrix_g[p.x]!!, p.y, "-")
            i = i + 1
        }
        var column: Int = (0).toInt()
        while (column < size) {
            matrix_g = move_x(matrix_g, column, size)
            column = column + 1
        }
        matrix_g = move_y(matrix_g, size)
    }
    var sc: Int = (increment_score(same_colors.size)).toInt()
    return PlayResult(matrix = matrix_g, score = sc)
}

fun build_matrix(matrix: MutableList<String>): MutableList<MutableList<String>> {
    var res: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var i: Int = (0).toInt()
    while (i < matrix.size) {
        var row: String = matrix[i]!!
        var row_list: MutableList<String> = mutableListOf<String>()
        var j: Int = (0).toInt()
        while (j < row.length) {
            row_list = run { val _tmp = row_list.toMutableList(); _tmp.add(row.substring(j, j + 1)); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row_list); _tmp }
        i = i + 1
    }
    return res
}

fun process_game(size: Int, matrix: MutableList<String>, moves: MutableList<Coord>): Int {
    var game_matrix: MutableList<MutableList<String>> = build_matrix(matrix)
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < moves.size) {
        var mv: Coord = moves[i]!!
        var res: PlayResult = play(game_matrix, mv.x, mv.y, size)
        game_matrix = res.matrix
        total = total + res.score
        i = i + 1
    }
    return total
}

fun user_main(): Unit {
    var size: Int = (4).toInt()
    var matrix: MutableList<String> = mutableListOf("RRBG", "RBBG", "YYGG", "XYGG")
    var moves: MutableList<Coord> = parse_moves("0 1,1 1")
    validate_matrix_size(size)
    validate_matrix_content(matrix, size)
    validate_moves(moves, size)
    var score: Int = (process_game(size, matrix, moves)).toInt()
    println(_numToStr(score))
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
