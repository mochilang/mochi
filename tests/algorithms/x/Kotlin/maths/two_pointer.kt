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

fun two_pointer(nums: MutableList<Int>, target: Int): MutableList<Int> {
    var i: Int = (0).toInt()
    var j: Int = (nums.size - 1).toInt()
    while (i < j) {
        var s: Int = (nums[i]!! + nums[j]!!).toInt()
        if (s == target) {
            return mutableListOf(i, j)
        }
        if (s < target) {
            i = i + 1
        } else {
            j = j - 1
        }
    }
    return mutableListOf<Int>()
}

fun test_two_pointer(): Unit {
    if (two_pointer(mutableListOf(2, 7, 11, 15), 9) != mutableListOf(0, 1)) {
        panic("case1")
    }
    if (two_pointer(mutableListOf(2, 7, 11, 15), 17) != mutableListOf(0, 3)) {
        panic("case2")
    }
    if (two_pointer(mutableListOf(2, 7, 11, 15), 18) != mutableListOf(1, 2)) {
        panic("case3")
    }
    if (two_pointer(mutableListOf(2, 7, 11, 15), 26) != mutableListOf(2, 3)) {
        panic("case4")
    }
    if (two_pointer(mutableListOf(1, 3, 3), 6) != mutableListOf(1, 2)) {
        panic("case5")
    }
    if ((two_pointer(mutableListOf(2, 7, 11, 15), 8)).size != 0) {
        panic("case6")
    }
    if ((two_pointer(mutableListOf(0, 3, 6, 9, 12, 15, 18, 21, 24, 27), 19)).size != 0) {
        panic("case7")
    }
    if ((two_pointer(mutableListOf(1, 2, 3), 6)).size != 0) {
        panic("case8")
    }
}

fun user_main(): Unit {
    test_two_pointer()
    println(two_pointer(mutableListOf(2, 7, 11, 15), 9))
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
