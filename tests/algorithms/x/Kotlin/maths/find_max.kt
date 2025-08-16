import java.math.BigInteger

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

fun normalize_index(index: Int, n: Int): Int {
    if (index < 0) {
        return n + index
    }
    return index
}

fun find_max_iterative(nums: MutableList<Double>): Double {
    if (nums.size == 0) {
        panic("find_max_iterative() arg is an empty sequence")
    }
    var max_num: Double = nums[0]!!
    var i: Int = (0).toInt()
    while (i < nums.size) {
        var x: Double = nums[i]!!
        if (x > max_num) {
            max_num = x
        }
        i = i + 1
    }
    return max_num
}

fun find_max_recursive(nums: MutableList<Double>, left: Int, right: Int): Double {
    var n: Int = (nums.size).toInt()
    if (n == 0) {
        panic("find_max_recursive() arg is an empty sequence")
    }
    if ((((((left >= n) || (left < (0 - n)) as Boolean)) || (right >= n) as Boolean)) || (right < (0 - n))) {
        panic("list index out of range")
    }
    var l: Int = (normalize_index(left, n)).toInt()
    var r: Int = (normalize_index(right, n)).toInt()
    if (l == r) {
        return nums[l]!!
    }
    var mid: Int = ((l + r) / 2).toInt()
    var left_max: Double = find_max_recursive(nums, l, mid)
    var right_max: Double = find_max_recursive(nums, mid + 1, r)
    if (left_max >= right_max) {
        return left_max
    }
    return right_max
}

fun test_find_max(): Unit {
    var arr: MutableList<Double> = mutableListOf(2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0)
    if (find_max_iterative(arr) != 94.0) {
        panic("find_max_iterative failed")
    }
    if (find_max_recursive(arr, 0, arr.size - 1) != 94.0) {
        panic("find_max_recursive failed")
    }
    if (find_max_recursive(arr, 0 - arr.size, 0 - 1) != 94.0) {
        panic("negative index handling failed")
    }
}

fun user_main(): Unit {
    test_find_max()
    var nums: MutableList<Double> = mutableListOf(2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0)
    println(find_max_iterative(nums))
    println(find_max_recursive(nums, 0, nums.size - 1))
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
