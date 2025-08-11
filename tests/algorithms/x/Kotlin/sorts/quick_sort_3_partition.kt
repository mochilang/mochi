fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
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

var array1: MutableList<Int> = mutableListOf(5, 0 - 1, 0 - 1, 5, 5, 24, 0)
fun quick_sort_3partition(arr: MutableList<Int>, left: Int, right: Int): MutableList<Int> {
    var arr: MutableList<Int> = arr
    if (right <= left) {
        return arr
    }
    var a: Int = (left).toInt()
    var i: Int = (left).toInt()
    var b: Int = (right).toInt()
    var pivot: Int = (arr[left]!!).toInt()
    while (i <= b) {
        if (arr[i]!! < pivot) {
            var temp: Int = (arr[a]!!).toInt()
            _listSet(arr, a, arr[i]!!)
            _listSet(arr, i, temp)
            a = a + 1
            i = i + 1
        } else {
            if (arr[i]!! > pivot) {
                var temp: Int = (arr[b]!!).toInt()
                _listSet(arr, b, arr[i]!!)
                _listSet(arr, i, temp)
                b = b - 1
            } else {
                i = i + 1
            }
        }
    }
    arr = quick_sort_3partition(arr, left, a - 1)
    arr = quick_sort_3partition(arr, b + 1, right)
    return arr
}

fun quick_sort_lomuto_partition(arr: MutableList<Int>, left: Int, right: Int): MutableList<Int> {
    var arr: MutableList<Int> = arr
    if (left < right) {
        var pivot_index: Int = (lomuto_partition(arr, left, right)).toInt()
        arr = quick_sort_lomuto_partition(arr, left, pivot_index - 1)
        arr = quick_sort_lomuto_partition(arr, pivot_index + 1, right)
    }
    return arr
}

fun lomuto_partition(arr: MutableList<Int>, left: Int, right: Int): Int {
    var pivot: Int = (arr[right]!!).toInt()
    var store_index: Int = (left).toInt()
    var i: Int = (left).toInt()
    while (i < right) {
        if (arr[i]!! < pivot) {
            var temp: Int = (arr[store_index]!!).toInt()
            _listSet(arr, store_index, arr[i]!!)
            _listSet(arr, i, temp)
            store_index = store_index + 1
        }
        i = i + 1
    }
    var temp: Int = (arr[right]!!).toInt()
    _listSet(arr, right, arr[store_index]!!)
    _listSet(arr, store_index, temp)
    return store_index
}

fun three_way_radix_quicksort(arr: MutableList<Int>): MutableList<Int> {
    if (arr.size <= 1) {
        return arr
    }
    var pivot: Int = (arr[0]!!).toInt()
    var less: MutableList<Int> = mutableListOf<Int>()
    var equal: MutableList<Int> = mutableListOf<Int>()
    var greater: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var _val: Int = (arr[i]!!).toInt()
        if (_val < pivot) {
            less = run { val _tmp = less.toMutableList(); _tmp.add(_val); _tmp }
        } else {
            if (_val > pivot) {
                greater = run { val _tmp = greater.toMutableList(); _tmp.add(_val); _tmp }
            } else {
                equal = run { val _tmp = equal.toMutableList(); _tmp.add(_val); _tmp }
            }
        }
        i = i + 1
    }
    var sorted_less: MutableList<Int> = three_way_radix_quicksort(less)
    var sorted_greater: MutableList<Int> = three_way_radix_quicksort(greater)
    var result = concat(sorted_less, equal)
    result = concat(result, sorted_greater)
    return (result as MutableList<Int>)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        array1 = quick_sort_3partition(array1, 0, array1.size - 1)
        println(array1.toString())
        var array2: MutableList<Int> = mutableListOf(9, 0, 2, 6)
        array2 = quick_sort_3partition(array2, 0, array2.size - 1)
        println(array2.toString())
        var array3: MutableList<Int> = mutableListOf<Int>()
        array3 = quick_sort_3partition(array3, 0, array3.size - 1)
        println(array3.toString())
        var nums1: MutableList<Int> = mutableListOf(0, 5, 3, 1, 2)
        nums1 = quick_sort_lomuto_partition(nums1, 0, nums1.size - 1)
        println(nums1.toString())
        var nums2: MutableList<Int> = mutableListOf<Int>()
        nums2 = quick_sort_lomuto_partition(nums2, 0, nums2.size - 1)
        println(nums2.toString())
        var nums3: MutableList<Int> = mutableListOf(0 - 2, 5, 0, 0 - 4)
        nums3 = quick_sort_lomuto_partition(nums3, 0, nums3.size - 1)
        println(nums3.toString())
        println(three_way_radix_quicksort(mutableListOf<Int>()).toString())
        println(three_way_radix_quicksort(mutableListOf(1)).toString())
        println(three_way_radix_quicksort(mutableListOf(0 - 5, 0 - 2, 1, 0 - 2, 0, 1)).toString())
        println(three_way_radix_quicksort(mutableListOf(1, 2, 5, 1, 2, 0, 0, 5, 2, 0 - 1)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
