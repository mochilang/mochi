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

var GLIDER: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 1, 0, 0, 0, 0, 0), mutableListOf(1, 1, 1, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 0))
var BLINKER: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0), mutableListOf(0, 1, 0), mutableListOf(0, 1, 0))
fun new_generation(cells: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var rows: Int = cells.size
    var cols: Int = (cells[0]!!).size
    var next: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < rows) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < cols) {
            var count: Int = 0
            if ((i > 0) && (j > 0)) {
                count = count + (((cells[i - 1]!!) as MutableList<Int>))[j - 1]!!
            }
            if (i > 0) {
                count = count + (((cells[i - 1]!!) as MutableList<Int>))[j]!!
            }
            if ((i > 0) && (j < (cols - 1))) {
                count = count + (((cells[i - 1]!!) as MutableList<Int>))[j + 1]!!
            }
            if (j > 0) {
                count = count + (((cells[i]!!) as MutableList<Int>))[j - 1]!!
            }
            if (j < (cols - 1)) {
                count = count + (((cells[i]!!) as MutableList<Int>))[j + 1]!!
            }
            if ((i < (rows - 1)) && (j > 0)) {
                count = count + (((cells[i + 1]!!) as MutableList<Int>))[j - 1]!!
            }
            if (i < (rows - 1)) {
                count = count + (((cells[i + 1]!!) as MutableList<Int>))[j]!!
            }
            if ((i < (rows - 1)) && (j < (cols - 1))) {
                count = count + (((cells[i + 1]!!) as MutableList<Int>))[j + 1]!!
            }
            var alive: Boolean = (((cells[i]!!) as MutableList<Int>))[j]!! == 1
            if (((((alive && (count >= 2) as Boolean)) && (count <= 3) as Boolean)) || (((!alive as Boolean) && (count == 3) as Boolean))) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            }
            j = j + 1
        }
        next = run { val _tmp = next.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return next
}

fun generate_generations(cells: MutableList<MutableList<Int>>, frames: Int): MutableList<MutableList<MutableList<Int>>> {
    var result: MutableList<MutableList<MutableList<Int>>> = mutableListOf<MutableList<MutableList<Int>>>()
    var i: Int = 0
    var current: MutableList<MutableList<Int>> = cells
    while (i < frames) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(current); _tmp }
        current = new_generation(current)
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var frames: MutableList<MutableList<MutableList<Int>>> = generate_generations(GLIDER, 4)
    var i: Int = 0
    while (i < frames.size) {
        println(frames[i]!!)
        i = i + 1
    }
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
