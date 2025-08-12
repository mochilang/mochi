import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
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

data class WordSearch(var words: MutableList<String> = mutableListOf<String>(), var width: Int = 0, var height: Int = 0, var board: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>())
var seed: Int = (123456789).toInt()
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed
}

fun rand_range(max: Int): Int {
    return Math.floorMod(rand(), max)
}

fun shuffle(list_int: MutableList<Int>): MutableList<Int> {
    var i: BigInteger = ((list_int.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) > 0) {
        var j: Int = (rand_range(((i.add((1).toBigInteger())).toInt()))).toInt()
        var tmp: Int = (list_int[(i).toInt()]!!).toInt()
        _listSet(list_int, (i).toInt(), list_int[j]!!)
        _listSet(list_int, j, tmp)
        i = i.subtract((1).toBigInteger())
    }
    return list_int
}

fun rand_letter(): String {
    var letters: String = "abcdefghijklmnopqrstuvwxyz"
    var i: Int = (rand_range(26)).toInt()
    return _sliceStr(letters, i, i + 1)
}

fun make_word_search(words: MutableList<String>, width: Int, height: Int): WordSearch {
    var board: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var r: Int = (0).toInt()
    while (r < height) {
        var row: MutableList<String> = mutableListOf<String>()
        var c: Int = (0).toInt()
        while (c < width) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(""); _tmp }
            c = c + 1
        }
        board = run { val _tmp = board.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    return WordSearch(words = words, width = width, height = height, board = board)
}

fun insert_dir(ws: WordSearch, word: String, dr: Int, dc: Int, rows: MutableList<Int>, cols: MutableList<Int>): Boolean {
    var word_len: Int = (word.length).toInt()
    var ri: Int = (0).toInt()
    while (ri < rows.size) {
        var row: Int = (rows[ri]!!).toInt()
        var ci: Int = (0).toInt()
        while (ci < cols.size) {
            var col: Int = (cols[ci]!!).toInt()
            var end_r: Int = (row + (dr * (word_len - 1))).toInt()
            var end_c: Int = (col + (dc * (word_len - 1))).toInt()
            if ((((((end_r < 0) || (end_r >= ws.height) as Boolean)) || (end_c < 0) as Boolean)) || (end_c >= ws.width)) {
                ci = ci + 1
                continue
            }
            var k: Int = (0).toInt()
            var ok: Boolean = true
            while (k < word_len) {
                var rr: Int = (row + (dr * k)).toInt()
                var cc: Int = (col + (dc * k)).toInt()
                if (((((ws.board)[rr]!!) as MutableList<String>))[cc]!! != "") {
                    ok = false
                    break
                }
                k = k + 1
            }
            if ((ok as Boolean)) {
                k = 0
                while (k < word_len) {
                    var rr2: Int = (row + (dr * k)).toInt()
                    var cc2: Int = (col + (dc * k)).toInt()
                    var row_list: MutableList<String> = (ws.board)[rr2]!!
                    _listSet(row_list, cc2, _sliceStr(word, k, k + 1))
                    k = k + 1
                }
                return true
            }
            ci = ci + 1
        }
        ri = ri + 1
    }
    return false
}

fun generate_board(ws: WordSearch): Unit {
    var dirs_r: MutableList<Int> = mutableListOf(0 - 1, 0 - 1, 0, 1, 1, 1, 0, 0 - 1)
    var dirs_c: MutableList<Int> = mutableListOf(0, 1, 1, 1, 0, 0 - 1, 0 - 1, 0 - 1)
    var i: Int = (0).toInt()
    while (i < (ws.words).size) {
        var word: String = (ws.words)[i]!!
        var rows: MutableList<Int> = mutableListOf<Int>()
        var r: Int = (0).toInt()
        while (r < ws.height) {
            rows = run { val _tmp = rows.toMutableList(); _tmp.add(r); _tmp }
            r = r + 1
        }
        var cols: MutableList<Int> = mutableListOf<Int>()
        var c: Int = (0).toInt()
        while (c < ws.width) {
            cols = run { val _tmp = cols.toMutableList(); _tmp.add(c); _tmp }
            c = c + 1
        }
        rows = shuffle(rows)
        cols = shuffle(cols)
        var d: Int = (rand_range(8)).toInt()
        insert_dir(ws, word, dirs_r[d]!!, dirs_c[d]!!, rows, cols)
        i = i + 1
    }
}

fun visualise(ws: WordSearch, add_fake_chars: Boolean): String {
    var result: String = ""
    var r: Int = (0).toInt()
    while (r < ws.height) {
        var c: Int = (0).toInt()
        while (c < ws.width) {
            var ch: String = ((((ws.board)[r]!!) as MutableList<String>))[c]!!
            if (ch == "") {
                if ((add_fake_chars as Boolean)) {
                    ch = rand_letter()
                } else {
                    ch = "#"
                }
            }
            result = (result + ch) + " "
            c = c + 1
        }
        result = result + "\n"
        r = r + 1
    }
    return result
}

fun user_main(): Unit {
    var words: MutableList<String> = mutableListOf("cat", "dog", "snake", "fish")
    var ws: WordSearch = make_word_search(words, 10, 10)
    generate_board(ws)
    println(visualise(ws, true))
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
