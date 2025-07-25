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
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

val dim: Int = 16
fun newPile(d: Int): MutableList<MutableList<Int>> {
    var b: MutableList<MutableList<Int>> = mutableListOf()
    var y: Int = 0
    while (y < d) {
        var row: MutableList<Int> = mutableListOf()
        var x: Int = 0
        while (x < d) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            x = x + 1
        }
        b = run { val _tmp = b.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        y = y + 1
    }
    return b
}

fun handlePile(pile: MutableList<MutableList<Int>>, x: Int, y: Int): MutableList<MutableList<Int>> {
    var pile: MutableList<MutableList<Int>> = pile
    if (pile[y][x] >= 4) {
        pile[y][x] = pile[y][x] - 4
        if (y > 0) {
            pile[y - 1][x] = pile[y - 1][x] + 1
            if (pile[y - 1][x] >= 4) {
                pile = handlePile(pile, x, y - 1)
            }
        }
        if (x > 0) {
            pile[y][x - 1] = pile[y][x - 1] + 1
            if (pile[y][x - 1] >= 4) {
                pile = handlePile(pile, x - 1, y)
            }
        }
        if (y < (dim - 1)) {
            pile[y + 1][x] = pile[y + 1][x] + 1
            if (pile[y + 1][x] >= 4) {
                pile = handlePile(pile, x, y + 1)
            }
        }
        if (x < (dim - 1)) {
            pile[y][x + 1] = pile[y][x + 1] + 1
            if (pile[y][x + 1] >= 4) {
                pile = handlePile(pile, x + 1, y)
            }
        }
        pile = handlePile(pile, x, y)
    }
    return pile
}

fun drawPile(pile: MutableList<MutableList<Int>>, d: Int): Unit {
    val chars: MutableList<String> = mutableListOf(" ", "░", "▓", "█")
    var row: Int = 0
    while (row < d) {
        var line: String = ""
        var col: Int = 0
        while (col < d) {
            var v: Int = pile[row][col]
            if (v > 3) {
                v = 3
            }
            line = line + chars[v]
            col = col + 1
        }
        println(line)
        row = row + 1
    }
}

fun user_main(): Unit {
    var pile: MutableList<MutableList<Int>> = newPile(16)
    val hdim: Int = 7
    pile[hdim][hdim] = 16
    pile = handlePile(pile, hdim, hdim)
    drawPile(pile, 16)
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
