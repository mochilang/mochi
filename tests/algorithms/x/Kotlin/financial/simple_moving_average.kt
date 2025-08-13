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

data class SMAValue(var value: Double = 0.0, var ok: Boolean = false)
var data: MutableList<Double> = mutableListOf(10.0, 12.0, 15.0, 13.0, 14.0, 16.0, 18.0, 17.0, 19.0, 21.0)
var window_size: Int = (3).toInt()
var sma_values: MutableList<SMAValue> = simple_moving_average(data, window_size)
var idx: Int = (0).toInt()
fun simple_moving_average(data: MutableList<Double>, window_size: Int): MutableList<SMAValue> {
    if (window_size < 1) {
        panic("Window size must be a positive integer")
    }
    var result: MutableList<SMAValue> = mutableListOf<SMAValue>()
    var window_sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < data.size) {
        window_sum = window_sum + data[i]!!
        if (i >= window_size) {
            window_sum = window_sum - data[i - window_size]!!
        }
        if (i >= (window_size - 1)) {
            var avg: Double = window_sum / (window_size).toDouble()
            result = run { val _tmp = result.toMutableList(); _tmp.add(SMAValue(value = avg, ok = true)); _tmp }
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(SMAValue(value = 0.0, ok = false)); _tmp }
        }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (idx < sma_values.size) {
            var item: SMAValue = sma_values[idx]!!
            if (((item.ok) as Boolean)) {
                println((("Day " + (idx + 1).toString()) + ": ") + item.value.toString())
            } else {
                println(("Day " + (idx + 1).toString()) + ": Not enough data for SMA")
            }
            idx = (idx + 1).toInt()
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
