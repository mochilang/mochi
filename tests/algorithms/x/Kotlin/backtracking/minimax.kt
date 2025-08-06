fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun minimax(depth: Int, node_index: Int, is_max: Boolean, scores: MutableList<Int>, height: Int): Int {
    if (depth < 0) {
        panic("Depth cannot be less than 0")
    }
    if (scores.size == 0) {
        panic("Scores cannot be empty")
    }
    if (depth == height) {
        return scores[node_index]!!
    }
    if ((is_max as Boolean)) {
        var left: Int = minimax(depth + 1, node_index * 2, false, scores, height)
        var right: Int = minimax(depth + 1, (node_index * 2) + 1, false, scores, height)
        if (left > right) {
            return left
        } else {
            return right
        }
    }
    var left: Int = minimax(depth + 1, node_index * 2, true, scores, height)
    var right: Int = minimax(depth + 1, (node_index * 2) + 1, true, scores, height)
    if (left < right) {
        return left
    } else {
        return right
    }
}

fun tree_height(n: Int): Int {
    var h: Int = 0
    var v: Int = n
    while (v > 1) {
        v = v / 2
        h = h + 1
    }
    return h
}

fun user_main(): Unit {
    var scores: MutableList<Int> = mutableListOf(90, 23, 6, 33, 21, 65, 123, 34423)
    var height: Int = tree_height(scores.size)
    println("Optimal value : " + minimax(0, 0, true, scores, height).toString())
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
