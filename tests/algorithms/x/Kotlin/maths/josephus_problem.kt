fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
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

var r: Int = (josephus_recursive(7, 3)).toInt()
fun josephus_recursive(num_people: Int, step_size: Int): Int {
    if ((num_people <= 0) || (step_size <= 0)) {
        panic("num_people or step_size is not a positive integer.")
    }
    if (num_people == 1) {
        return 0
    }
    return Math.floorMod((josephus_recursive(num_people - 1, step_size) + step_size), num_people)
}

fun find_winner(num_people: Int, step_size: Int): Int {
    return josephus_recursive(num_people, step_size) + 1
}

fun remove_at(xs: MutableList<Int>, idx: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i != idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun josephus_iterative(num_people: Int, step_size: Int): Int {
    if ((num_people <= 0) || (step_size <= 0)) {
        panic("num_people or step_size is not a positive integer.")
    }
    var circle: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i <= num_people) {
        circle = run { val _tmp = circle.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    var current: Int = (0).toInt()
    while (circle.size > 1) {
        current = Math.floorMod(((current + step_size) - 1), circle.size)
        circle = remove_at(circle, current)
    }
    return circle[0]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(r))
        println(_numToStr(find_winner(7, 3)))
        println(_numToStr(josephus_iterative(7, 3)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
