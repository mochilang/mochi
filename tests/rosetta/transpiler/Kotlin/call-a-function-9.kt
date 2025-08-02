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

fun f(): MutableList<Any?> {
    return mutableListOf<Any?>(0 as Any?, 0.0 as Any?)
}

fun g(a: Int, b: Double): Int {
    return 0
}

fun h(s: String, nums: MutableList<Int>): Unit {
}

fun user_main(): Unit {
    var ab: MutableList<Any?> = f()
    var a: Any? = ab[0] as Any?
    var b: Any? = ab[1] as Any?
    var cb: Any? = (f())[1] as Any?
    var d: Int = g(a as Int, cb as Double)
    var e: Int = g(d, b as Double)
    var i: Int = g(d, 2.0)
    var list: MutableList<Int> = mutableListOf<Int>()
    list = run { val _tmp = list.toMutableList(); _tmp.add(a as Int); _tmp } as MutableList<Int>
    list = run { val _tmp = list.toMutableList(); _tmp.add(d); _tmp } as MutableList<Int>
    list = run { val _tmp = list.toMutableList(); _tmp.add(e); _tmp } as MutableList<Int>
    list = run { val _tmp = list.toMutableList(); _tmp.add(i); _tmp } as MutableList<Int>
    i = list.size
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
