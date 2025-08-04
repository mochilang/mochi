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

fun user_main(): Unit {
    var row: Int = 3
    var col: Int = 4
    var a: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < row) {
        var rowArr: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < col) {
            rowArr = run { val _tmp = rowArr.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        a = run { val _tmp = a.toMutableList(); _tmp.add(rowArr); _tmp }
        i = i + 1
    }
    println("a[0][0] = " + ((((a[0]!!) as MutableList<Int>))[0]!!).toString())
    (a[((row - 1).toInt())]!!)[((col - 1).toInt())] = 7
    println((((("a[" + (row - 1).toString()) + "][") + (col - 1).toString()) + "] = ") + ((((a[((row - 1).toInt())]!!) as MutableList<Int>))[((col - 1).toInt())]!!).toString())
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
