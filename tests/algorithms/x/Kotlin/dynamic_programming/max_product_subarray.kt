import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

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

fun max_product_subarray(numbers: MutableList<Int>): Int {
    if (numbers.size == 0) {
        return 0
    }
    var max_till_now: Int = (numbers[0]!!).toInt()
    var min_till_now: Int = (numbers[0]!!).toInt()
    var max_prod: Int = (numbers[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < numbers.size) {
        var number: Int = (numbers[i]!!).toInt()
        if (number < 0) {
            var temp: Int = (max_till_now).toInt()
            max_till_now = min_till_now
            min_till_now = temp
        }
        var prod_max: Int = (max_till_now * number).toInt()
        if (number > prod_max) {
            max_till_now = number
        } else {
            max_till_now = prod_max
        }
        var prod_min: Int = (min_till_now * number).toInt()
        if (number < prod_min) {
            min_till_now = number
        } else {
            min_till_now = prod_min
        }
        if (max_till_now > max_prod) {
            max_prod = max_till_now
        }
        i = i + 1
    }
    return max_prod
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(max_product_subarray(mutableListOf(2, 3, 0 - 2, 4)))
        println(max_product_subarray(mutableListOf(0 - 2, 0, 0 - 1)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
