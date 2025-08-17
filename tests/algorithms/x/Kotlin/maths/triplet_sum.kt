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

fun bubble_sort(nums: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
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
                var tmp: Int = (arr[b]!!).toInt()
                _listSet(arr, b, arr[b + 1]!!)
                _listSet(arr, b + 1, tmp)
            }
            b = b + 1
        }
        a = a + 1
    }
    return arr
}

fun sort3(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    var n: Int = (arr.size).toInt()
    var a: Int = (0).toInt()
    while (a < n) {
        var b: Int = (0).toInt()
        while (b < ((n - a) - 1)) {
            if (arr[b]!! > arr[b + 1]!!) {
                var tmp: Int = (arr[b]!!).toInt()
                _listSet(arr, b, arr[b + 1]!!)
                _listSet(arr, b + 1, tmp)
            }
            b = b + 1
        }
        a = a + 1
    }
    return arr
}

fun triplet_sum1(arr: MutableList<Int>, target: Int): MutableList<Int> {
    var i: Int = (0).toInt()
    while (i < (arr.size - 2)) {
        var j: Int = (i + 1).toInt()
        while (j < (arr.size - 1)) {
            var k: Int = (j + 1).toInt()
            while (k < arr.size) {
                if (((arr[i]!! + arr[j]!!) + arr[k]!!) == target) {
                    return sort3(mutableListOf(arr[i]!!, arr[j]!!, arr[k]!!))
                }
                k = k + 1
            }
            j = j + 1
        }
        i = i + 1
    }
    return mutableListOf(0, 0, 0)
}

fun triplet_sum2(arr: MutableList<Int>, target: Int): MutableList<Int> {
    var sorted: MutableList<Int> = bubble_sort(arr)
    var n: Int = (sorted.size).toInt()
    var i: Int = (0).toInt()
    while (i < (n - 2)) {
        var left: Int = (i + 1).toInt()
        var right: Int = (n - 1).toInt()
        while (left < right) {
            var s: Int = ((sorted[i]!! + sorted[left]!!) + sorted[right]!!).toInt()
            if (s == target) {
                return mutableListOf(sorted[i]!!, sorted[left]!!, sorted[right]!!)
            }
            if (s < target) {
                left = left + 1
            } else {
                right = right - 1
            }
        }
        i = i + 1
    }
    return mutableListOf(0, 0, 0)
}

fun list_equal(a: MutableList<Int>, b: MutableList<Int>): Boolean {
    if (a.size != b.size) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < a.size) {
        if (a[i]!! != b[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun test_triplet_sum(): Unit {
    var arr1: MutableList<Int> = mutableListOf(13, 29, 7, 23, 5)
    if (!list_equal(triplet_sum1(arr1, 35), mutableListOf(5, 7, 23))) {
        panic("ts1 case1 failed")
    }
    if (!list_equal(triplet_sum2(arr1, 35), mutableListOf(5, 7, 23))) {
        panic("ts2 case1 failed")
    }
    var arr2: MutableList<Int> = mutableListOf(37, 9, 19, 50, 44)
    if (!list_equal(triplet_sum1(arr2, 65), mutableListOf(9, 19, 37))) {
        panic("ts1 case2 failed")
    }
    if (!list_equal(triplet_sum2(arr2, 65), mutableListOf(9, 19, 37))) {
        panic("ts2 case2 failed")
    }
    var arr3: MutableList<Int> = mutableListOf(6, 47, 27, 1, 15)
    if (!list_equal(triplet_sum1(arr3, 11), mutableListOf(0, 0, 0))) {
        panic("ts1 case3 failed")
    }
    if (!list_equal(triplet_sum2(arr3, 11), mutableListOf(0, 0, 0))) {
        panic("ts2 case3 failed")
    }
}

fun user_main(): Unit {
    test_triplet_sum()
    var sample: MutableList<Int> = mutableListOf(13, 29, 7, 23, 5)
    var res: MutableList<Int> = triplet_sum2(sample, 35)
    println((((_numToStr(res[0]!!) + " ") + _numToStr(res[1]!!)) + " ") + _numToStr(res[2]!!))
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
