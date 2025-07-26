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

fun each(xs: MutableList<Int>, f: (Int) -> Any): Unit {
    for (x in xs) {
        f(x)
    }
}

fun Map(xs: MutableList<Int>, f: (Int) -> Int): MutableList<Int> {
    var r: MutableList<Int> = mutableListOf()
    for (x in xs) {
        r = run { val _tmp = r.toMutableList(); _tmp.add(f(x)); _tmp } as MutableList<Int>
    }
    return r
}

fun user_main(): Unit {
    val s: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5)
    each(s, { i: Int -> println((i * i).toString()) } as (Int) -> Any)
    println(Map(s, { i: Int -> i * i } as (Int) -> Int).toString())
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
