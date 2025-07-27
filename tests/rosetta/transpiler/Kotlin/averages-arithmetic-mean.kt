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

fun mean(v: MutableList<Double>): MutableMap<String, Any?> {
    if (v.size == 0) {
        return mutableMapOf<String, Boolean>("ok" to (false)) as MutableMap<String, Any?>
    }
    var sum: Double = 0.0
    var i: Int = 0
    while (i < v.size) {
        sum = sum + v[i]
        i = i + 1
    }
    return mutableMapOf<String, Any?>("ok" to (true), "mean" to (sum / v.size.toDouble()))
}

fun user_main(): Unit {
    val sets = mutableListOf(mutableListOf<Double>(), mutableListOf(3.0, 1.0, 4.0, 1.0, 5.0, 9.0), mutableListOf(100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 0.0 - 100000000000000000000.0), mutableListOf(10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11), mutableListOf(10.0, 20.0, 30.0, 40.0, 50.0, 0.0 - 100.0, 4.7, 0.0 - 1100.0))
    for (v in sets) {
        println("Vector: " + v.toString())
        val r: MutableMap<String, Any?> = mean(v as MutableList<Double>)
        if (((r)["ok"]) as Boolean) {
            println((("Mean of " + v.size.toString()) + " numbers is ") + ((r)["mean"]).toString())
        } else {
            println("Mean undefined")
        }
        println("")
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
