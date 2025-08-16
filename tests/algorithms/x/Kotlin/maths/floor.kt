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

fun floor(x: Double): Int {
    var i: Int = ((x.toInt())).toInt()
    if ((x - ((i.toDouble()))) >= 0.0) {
        return i
    }
    return i - 1
}

fun test_floor(): Unit {
    var nums: MutableList<Double> = mutableListOf(1.0, 0.0 - 1.0, 0.0, 0.0, 1.1, 0.0 - 1.1, 1.0, 0.0 - 1.0, 1000000000.0)
    var expected: MutableList<Int> = mutableListOf(1, 0 - 1, 0, 0, 1, 0 - 2, 1, 0 - 1, 1000000000)
    var idx: Int = (0).toInt()
    while (idx < nums.size) {
        if (floor(nums[idx]!!) != expected[idx]!!) {
            panic("floor test failed")
        }
        idx = idx + 1
    }
}

fun user_main(): Unit {
    test_floor()
    println(_numToStr(floor(0.0 - 1.1)))
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
