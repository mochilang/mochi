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

fun tail(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun rotate_left(xs: MutableList<Int>): MutableList<Int> {
    if (xs.size == 0) {
        return xs
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(xs[0]!!); _tmp }
    return res
}

fun permute_recursive(nums: MutableList<Int>): MutableList<MutableList<Int>> {
    if (nums.size == 0) {
        var base: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
        return run { val _tmp = base.toMutableList(); _tmp.add(mutableListOf<Int>()); _tmp }
    }
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var current: MutableList<Int> = nums
    var count: Int = (0).toInt()
    while (count < nums.size) {
        var n: Int = (current[0]!!).toInt()
        var rest: MutableList<Int> = tail(current)
        var perms: MutableList<MutableList<Int>> = permute_recursive(rest)
        var j: Int = (0).toInt()
        while (j < perms.size) {
            var perm = run { val _tmp = (perms[j]!!).toMutableList(); _tmp.add(n); _tmp }
            result = run { val _tmp = result.toMutableList(); _tmp.add((perm as MutableList<Int>)); _tmp }
            j = j + 1
        }
        current = rotate_left(current)
        count = count + 1
    }
    return result
}

fun swap(xs: MutableList<Int>, i: Int, j: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (0).toInt()
    while (k < xs.size) {
        if (k == i) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[j]!!); _tmp }
        } else {
            if (k == j) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
            } else {
                res = run { val _tmp = res.toMutableList(); _tmp.add(xs[k]!!); _tmp }
            }
        }
        k = k + 1
    }
    return res
}

fun permute_backtrack_helper(nums: MutableList<Int>, start: Int, output: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    if (start == (nums.size - 1)) {
        return run { val _tmp = output.toMutableList(); _tmp.add(nums); _tmp }
    }
    var i: Int = (start).toInt()
    var res: MutableList<MutableList<Int>> = output
    while (i < nums.size) {
        var swapped: MutableList<Int> = swap(nums, start, i)
        res = permute_backtrack_helper(swapped, start + 1, res)
        i = i + 1
    }
    return res
}

fun permute_backtrack(nums: MutableList<Int>): MutableList<MutableList<Int>> {
    var output: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    return permute_backtrack_helper(nums, 0, output)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(permute_recursive(mutableListOf(1, 2, 3)).toString())
        println(permute_backtrack(mutableListOf(1, 2, 3)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
