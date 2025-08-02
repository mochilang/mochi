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

var grid: MutableList<MutableList<String>> = mutableListOf(mutableListOf(".", ".", ".", ".", "."), mutableListOf(".", "#", "#", "#", "."), mutableListOf(".", "#", ".", "#", "."), mutableListOf(".", "#", "#", "#", "."), mutableListOf(".", ".", ".", ".", "."))
fun flood(x: Int, y: Int, repl: String): Unit {
    var target: String = (((grid[y]!!) as MutableList<String>))[x]!!
    if (target == repl) {
        return
    }
    fun ff(px: Int, py: Int): Unit {
        if ((((((px < 0) || (py < 0) as Boolean)) || (py >= grid.size) as Boolean)) || (px >= (grid[0]!!).size)) {
            return
        }
        if ((((grid[py]!!) as MutableList<String>))[px]!! != target) {
            return
        }
        (grid[py]!!)[px] = repl
        ff(px - 1, py)
        ff(px + 1, py)
        ff(px, py - 1)
        ff(px, py + 1)
    }

    ff(x, y)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        flood(2, 2, "o")
        for (row in grid) {
            var line: String = ""
            for (ch in row) {
                line = line + ch
            }
            println(line)
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
