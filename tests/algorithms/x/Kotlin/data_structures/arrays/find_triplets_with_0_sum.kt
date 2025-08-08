import java.math.BigInteger

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

fun sort_triplet(a: Int, b: Int, c: Int): MutableList<Int> {
    var x: Int = a
    var y: Int = b
    var z: Int = c
    if (x > y) {
        var t: Int = x
        x = y
        y = t
    }
    if (y > z) {
        var t: Int = y
        y = z
        z = t
    }
    if (x > y) {
        var t: Int = x
        x = y
        y = t
    }
    return mutableListOf(x, y, z)
}

fun contains_triplet(arr: MutableList<MutableList<Int>>, target: MutableList<Int>): Boolean {
    for (i in 0 until arr.size) {
        var item: MutableList<Int> = arr[i]!!
        var same: Boolean = true
        for (j in 0 until target.size) {
            if (item[j]!! != target[j]!!) {
                same = false
                break
            }
        }
        if ((same as Boolean)) {
            return true
        }
    }
    return false
}

fun contains_int(arr: MutableList<Int>, value: Int): Boolean {
    for (i in 0 until arr.size) {
        if (arr[i]!! == value) {
            return true
        }
    }
    return false
}

fun find_triplets_with_0_sum(nums: MutableList<Int>): MutableList<MutableList<Int>> {
    var n: Int = nums.size
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (i in 0 until n) {
        for (j in i + 1 until n) {
            for (k in j + 1 until n) {
                var a: Int = nums[i]!!
                var b: Int = nums[j]!!
                var c: Int = nums[k]!!
                if (((a + b) + c) == 0) {
                    var trip: MutableList<Int> = sort_triplet(a, b, c)
                    if (!contains_triplet(result, trip)) {
                        result = run { val _tmp = result.toMutableList(); _tmp.add(trip); _tmp }
                    }
                }
            }
        }
    }
    return result
}

fun find_triplets_with_0_sum_hashing(arr: MutableList<Int>): MutableList<MutableList<Int>> {
    var target_sum: Int = 0
    var output: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (i in 0 until arr.size) {
        var seen: MutableList<Int> = mutableListOf<Int>()
        var current_sum: Int = target_sum - arr[i]!!
        for (j in i + 1 until arr.size) {
            var other: Int = arr[j]!!
            var required: Int = current_sum - other
            if (((contains_int(seen, required)) as Boolean)) {
                var trip: MutableList<Int> = sort_triplet(arr[i]!!, other, required)
                if (!contains_triplet(output, trip)) {
                    output = run { val _tmp = output.toMutableList(); _tmp.add(trip); _tmp }
                }
            }
            seen = run { val _tmp = seen.toMutableList(); _tmp.add(other); _tmp }
        }
    }
    return output
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_triplets_with_0_sum(mutableListOf(0 - 1, 0, 1, 2, 0 - 1, 0 - 4)).toString())
        println(find_triplets_with_0_sum(mutableListOf<Int>()).toString())
        println(find_triplets_with_0_sum(mutableListOf(0, 0, 0)).toString())
        println(find_triplets_with_0_sum(mutableListOf(1, 2, 3, 0, 0 - 1, 0 - 2, 0 - 3)).toString())
        println(find_triplets_with_0_sum_hashing(mutableListOf(0 - 1, 0, 1, 2, 0 - 1, 0 - 4)).toString())
        println(find_triplets_with_0_sum_hashing(mutableListOf<Int>()).toString())
        println(find_triplets_with_0_sum_hashing(mutableListOf(0, 0, 0)).toString())
        println(find_triplets_with_0_sum_hashing(mutableListOf(1, 2, 3, 0, 0 - 1, 0 - 2, 0 - 3)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
