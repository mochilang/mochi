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

var graph: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0, 0, 0), mutableListOf(1, 0, 1, 0, 1), mutableListOf(0, 1, 0, 1, 0), mutableListOf(0, 1, 1, 0, 0), mutableListOf(0, 1, 0, 0, 0))
fun valid_coloring(neighbours: MutableList<Int>, colored_vertices: MutableList<Int>, color: Int): Boolean {
    var i: Int = 0
    while (i < neighbours.size) {
        if ((neighbours[i]!! == 1) && (colored_vertices[i]!! == color)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun util_color(graph: MutableList<MutableList<Int>>, max_colors: Int, colored_vertices: MutableList<Int>, index: Int): Boolean {
    if (index == graph.size) {
        return true
    }
    var c: Int = 0
    while (c < max_colors) {
        if (((valid_coloring(graph[index]!!, colored_vertices, c)) as Boolean)) {
            colored_vertices[index] = c
            if (((util_color(graph, max_colors, colored_vertices, index + 1)) as Boolean)) {
                return true
            }
            colored_vertices[index] = 0 - 1
        }
        c = c + 1
    }
    return false
}

fun color(graph: MutableList<MutableList<Int>>, max_colors: Int): MutableList<Int> {
    var colored_vertices: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < graph.size) {
        colored_vertices = run { val _tmp = colored_vertices.toMutableList(); _tmp.add(0 - 1); _tmp }
        i = i + 1
    }
    if (((util_color(graph, max_colors, colored_vertices, 0)) as Boolean)) {
        return colored_vertices
    }
    return mutableListOf<Int>()
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(color(graph, 3))
        println("\n")
        println((color(graph, 2)).size)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
