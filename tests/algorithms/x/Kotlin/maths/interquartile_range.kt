import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun bubble_sort(nums: MutableList<Double>): MutableList<Double> {
    var arr: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < nums.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(nums[i]!!); _tmp }
        i = i + 1
    }
    var n: Int = (arr.size).toInt()
    var a: Int = (0).toInt()
    while (a < n) {
        var b: Int = (0).toInt()
        while (b < ((n - a) - 1)) {
            if (arr[b]!! > arr[b + 1]!!) {
                var temp: Double = arr[b]!!
                _listSet(arr, b, arr[b + 1]!!)
                _listSet(arr, b + 1, temp)
            }
            b = b + 1
        }
        a = a + 1
    }
    return arr
}

fun find_median(nums: MutableList<Double>): Double {
    var length: Int = (nums.size).toInt()
    var div: Int = (length / 2).toInt()
    var mod: Int = (Math.floorMod(length, 2)).toInt()
    if (mod != 0) {
        return nums[div]!!
    }
    return (nums[div]!! + nums[div - 1]!!) / 2.0
}

fun interquartile_range(nums: MutableList<Double>): Double {
    if (nums.size == 0) {
        panic("The list is empty. Provide a non-empty list.")
    }
    var sorted: MutableList<Double> = bubble_sort(nums)
    var length: Int = (sorted.size).toInt()
    var div: Int = (length / 2).toInt()
    var mod: Int = (Math.floorMod(length, 2)).toInt()
    var lower: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < div) {
        lower = run { val _tmp = lower.toMutableList(); _tmp.add(sorted[i]!!); _tmp }
        i = i + 1
    }
    var upper: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (div + mod).toInt()
    while (j < length) {
        upper = run { val _tmp = upper.toMutableList(); _tmp.add(sorted[j]!!); _tmp }
        j = j + 1
    }
    var q1: Double = find_median(lower)
    var q3: Double = find_median(upper)
    return q3 - q1
}

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun float_equal(a: Double, b: Double): Boolean {
    var diff: Double = absf(a - b)
    return diff < 0.0000001
}

fun test_interquartile_range(): Unit {
    if (!float_equal(interquartile_range(mutableListOf(4.0, 1.0, 2.0, 3.0, 2.0)), 2.0)) {
        panic("interquartile_range case1 failed")
    }
    if (!float_equal(interquartile_range(mutableListOf(0.0 - 2.0, 0.0 - 7.0, 0.0 - 10.0, 9.0, 8.0, 4.0, 0.0 - 67.0, 45.0)), 17.0)) {
        panic("interquartile_range case2 failed")
    }
    if (!float_equal(interquartile_range(mutableListOf(0.0 - 2.1, 0.0 - 7.1, 0.0 - 10.1, 9.1, 8.1, 4.1, 0.0 - 67.1, 45.1)), 17.2)) {
        panic("interquartile_range case3 failed")
    }
    if (!float_equal(interquartile_range(mutableListOf(0.0, 0.0, 0.0, 0.0, 0.0)), 0.0)) {
        panic("interquartile_range case4 failed")
    }
}

fun user_main(): Unit {
    test_interquartile_range()
    println(_numToStr(interquartile_range(mutableListOf(4.0, 1.0, 2.0, 3.0, 2.0))))
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
