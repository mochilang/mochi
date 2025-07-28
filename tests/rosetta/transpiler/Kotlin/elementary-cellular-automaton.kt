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

fun bitAt(x: Int, idx: Int): Int {
    var v: Int = x
    var i: Int = 0
    while (i < idx) {
        v = (v / 2).toInt()
        i = i + 1
    }
    return Math.floorMod(v, 2)
}

fun outputState(state: String): Unit {
    var line: String = ""
    var i: Int = 0
    while (i < state.length) {
        if (state.substring(i, i + 1) == "1") {
            line = line + "#"
        } else {
            line = line + " "
        }
        i = i + 1
    }
    println(line)
}

fun step(state: String, r: Int): String {
    val cells: Int = state.length
    var out: String = ""
    var i: Int = 0
    while (i < cells) {
        val l: String = state.substring(Math.floorMod(((i - 1) + cells), cells), (Math.floorMod(((i - 1) + cells), cells)) + 1)
        val c: String = state.substring(i, i + 1)
        val rt: String = state.substring(Math.floorMod((i + 1), cells), (Math.floorMod((i + 1), cells)) + 1)
        var idx: Int = 0
        if (l == "1") {
            idx = idx + 4
        }
        if (c == "1") {
            idx = idx + 2
        }
        if (rt == "1") {
            idx = idx + 1
        }
        if (bitAt(r, idx) == 1) {
            out = out + "1"
        } else {
            out = out + "0"
        }
        i = i + 1
    }
    return out
}

fun elem(r: Int, cells: Int, generations: Int, state: String): Unit {
    outputState(state)
    var g: Int = 0
    var s: String = state
    while (g < generations) {
        s = step(s, r)
        outputState(s)
        g = g + 1
    }
}

fun randInit(cells: Int, seed: Int): String {
    var s: String = ""
    var _val: Int = seed
    var i: Int = 0
    while (i < cells) {
        _val = Math.floorMod(((_val * 1664525) + 1013904223), 2147483647)
        if ((Math.floorMod(_val, 2)) == 0) {
            s = s + "0"
        } else {
            s = s + "1"
        }
        i = i + 1
    }
    return s
}

fun singleInit(cells: Int): String {
    var s: String = ""
    var i: Int = 0
    while (i < cells) {
        if (i == (cells / 2)) {
            s = s + "1"
        } else {
            s = s + "0"
        }
        i = i + 1
    }
    return s
}

fun user_main(): Unit {
    val cells: Int = 20
    val generations: Int = 9
    println("Single 1, rule 90:")
    var state: String = singleInit(cells)
    elem(90, cells, generations, state)
    println("Random intial state, rule 30:")
    state = randInit(cells, 3)
    elem(30, cells, generations, state)
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
