import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun sort_list(nums: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = nums
    var i: Int = (1).toInt()
    while (i < arr.size) {
        var key: Int = (arr[i]!!).toInt()
        var j: Int = (i - 1).toInt()
        while ((j >= 0) && (arr[j]!! > key)) {
            _listSet(arr, j + 1, arr[j]!!)
            j = j - 1
        }
        _listSet(arr, j + 1, key)
        i = i + 1
    }
    return arr
}

fun largest_divisible_subset(items: MutableList<Int>): MutableList<Int> {
    if (items.size == 0) {
        return mutableListOf<Int>()
    }
    var nums: MutableList<Int> = sort_list(items)
    var n: Int = (nums.size).toInt()
    var memo: MutableList<Int> = mutableListOf<Int>()
    var prev: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        memo = run { val _tmp = memo.toMutableList(); _tmp.add(1); _tmp }
        prev = run { val _tmp = prev.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    i = 0
    while (i < n) {
        var j: Int = (0).toInt()
        while (j < i) {
            if ((((nums[j]!! == 0) || ((Math.floorMod(nums[i]!!, nums[j]!!)) == 0) as Boolean)) && ((memo[j]!! + 1) > memo[i]!!)) {
                _listSet(memo, i, memo[j]!! + 1)
                _listSet(prev, i, j)
            }
            j = j + 1
        }
        i = i + 1
    }
    var ans: Int = (0 - 1).toInt()
    var last_index: Int = (0 - 1).toInt()
    i = 0
    while (i < n) {
        if (memo[i]!! > ans) {
            ans = memo[i]!!
            last_index = i
        }
        i = i + 1
    }
    if (last_index == (0 - 1)) {
        return mutableListOf<Int>()
    }
    var result: MutableList<Int> = mutableListOf(nums[last_index]!!)
    while (prev[last_index]!! != last_index) {
        last_index = prev[last_index]!!
        result = run { val _tmp = result.toMutableList(); _tmp.add(nums[last_index]!!); _tmp }
    }
    return result
}

fun user_main(): Unit {
    var items: MutableList<Int> = mutableListOf(1, 16, 7, 8, 4)
    var subset: MutableList<Int> = largest_divisible_subset(items)
    println(((("The longest divisible subset of " + items.toString()) + " is ") + subset.toString()) + ".")
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
