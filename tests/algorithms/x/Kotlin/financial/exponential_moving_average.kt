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

var stock_prices: MutableList<Double> = mutableListOf(2.0, 5.0, 3.0, 8.2, 6.0, 9.0, 10.0)
var window_size: Int = (3).toInt()
var result: MutableList<Double> = exponential_moving_average(stock_prices, window_size)
fun exponential_moving_average(stock_prices: MutableList<Double>, window_size: Int): MutableList<Double> {
    if (window_size <= 0) {
        panic("window_size must be > 0")
    }
    var alpha: Double = 2.0 / (1.0 + ((window_size.toDouble())))
    var moving_average: Double = 0.0
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < stock_prices.size) {
        var price: Double = stock_prices[i]!!
        if (i <= window_size) {
            if (i == 0) {
                moving_average = price
            } else {
                moving_average = (moving_average + price) * 0.5
            }
        } else {
            moving_average = (alpha * price) + ((1.0 - alpha) * moving_average)
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(moving_average); _tmp }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(result.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
