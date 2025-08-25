val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

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

fun max_subarray_sum(nums: MutableList<Double>, allow_empty: Boolean): Double {
    if (nums.size == 0) {
        return 0.0
    }
    var max_sum: Double = 0.0
    var curr_sum: Double = 0.0
    if (allow_empty as Boolean) {
        max_sum = 0.0
        curr_sum = 0.0
        var i: Int = (0).toInt()
        while (i < nums.size) {
            var num: Double = nums[i]!!
            var temp: Double = curr_sum + num
            curr_sum = if (temp > 0.0) temp else 0.0.toDouble()
            if (curr_sum > max_sum) {
                max_sum = curr_sum
            }
            i = i + 1
        }
    } else {
        max_sum = nums[0]!!
        curr_sum = nums[0]!!
        var i: Int = (1).toInt()
        while (i < nums.size) {
            var num: Double = nums[i]!!
            var temp: Double = curr_sum + num
            curr_sum = if (temp > num) temp else num.toDouble()
            if (curr_sum > max_sum) {
                max_sum = curr_sum
            }
            i = i + 1
        }
    }
    return max_sum
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(max_subarray_sum(mutableListOf(2.0, 8.0, 9.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(0.0, 0.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(0.0 - 1.0, 0.0, 1.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(1.0, 2.0, 3.0, 4.0, 0.0 - 2.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(0.0 - 2.0, 1.0, 0.0 - 3.0, 4.0, 0.0 - 1.0, 2.0, 1.0, 0.0 - 5.0, 4.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(2.0, 3.0, 0.0 - 9.0, 8.0, 0.0 - 2.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(0.0 - 2.0, 0.0 - 3.0, 0.0 - 1.0, 0.0 - 4.0, 0.0 - 6.0), false)))
        println(_numToStr(max_subarray_sum(mutableListOf(0.0 - 2.0, 0.0 - 3.0, 0.0 - 1.0, 0.0 - 4.0, 0.0 - 6.0), true)))
        var empty: MutableList<Double> = mutableListOf<Double>()
        println(_numToStr(max_subarray_sum(empty, false)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
