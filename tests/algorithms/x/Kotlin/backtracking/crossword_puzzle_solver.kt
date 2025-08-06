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

var puzzle: MutableList<MutableList<String>> = mutableListOf(mutableListOf("", "", ""), mutableListOf("", "", ""), mutableListOf("", "", ""))
var words: MutableList<String> = mutableListOf("cat", "dog", "car")
var used: MutableList<Boolean> = mutableListOf(false, false, false)
fun is_valid(puzzle: MutableList<MutableList<String>>, word: String, row: Int, col: Int, vertical: Boolean): Boolean {
    for (i in 0 until word.length) {
        if ((vertical as Boolean)) {
            if (((row + i) >= puzzle.size) || ((((puzzle[row + i]!!) as MutableList<String>))[col]!! != "")) {
                return false
            }
        } else {
            if (((col + i) >= (puzzle[0]!!).size) || ((((puzzle[row]!!) as MutableList<String>))[col + i]!! != "")) {
                return false
            }
        }
    }
    return true
}

fun place_word(puzzle: MutableList<MutableList<String>>, word: String, row: Int, col: Int, vertical: Boolean): Unit {
    for (i in 0 until word.length) {
        var ch: String = word[i].toString()
        if ((vertical as Boolean)) {
            (puzzle[row + i]!!)[col] = ch
        } else {
            (puzzle[row]!!)[col + i] = ch
        }
    }
}

fun remove_word(puzzle: MutableList<MutableList<String>>, word: String, row: Int, col: Int, vertical: Boolean): Unit {
    for (i in 0 until word.length) {
        if ((vertical as Boolean)) {
            (puzzle[row + i]!!)[col] = ""
        } else {
            (puzzle[row]!!)[col + i] = ""
        }
    }
}

fun solve_crossword(puzzle: MutableList<MutableList<String>>, words: MutableList<String>, used: MutableList<Boolean>): Boolean {
    for (row in 0 until puzzle.size) {
        for (col in 0 until (puzzle[0]!!).size) {
            if ((((puzzle[row]!!) as MutableList<String>))[col]!! == "") {
                for (i in 0 until words.size) {
                    if (!((used[i]!!) as? Boolean ?: false)) {
                        var word: String = words[i]!!
                        for (vertical in mutableListOf(true, false)) {
                            if (((is_valid(puzzle, word, row, col, vertical)) as Boolean)) {
                                place_word(puzzle, word, row, col, vertical)
                                used[i] = true
                                if (((solve_crossword(puzzle, words, used)) as Boolean)) {
                                    return true
                                }
                                used[i] = false
                                remove_word(puzzle, word, row, col, vertical)
                            }
                        }
                    }
                }
                return false
            }
        }
    }
    return true
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        if (((solve_crossword(puzzle, words, used)) as Boolean)) {
            println("Solution found:")
            for (row in puzzle) {
                println(row)
            }
        } else {
            println("No solution found:")
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
