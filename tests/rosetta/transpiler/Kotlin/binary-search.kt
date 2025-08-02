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

fun bsearch(arr: MutableList<Int>, x: Int): Int {
    var low: Int = 0
    var high: BigInteger = (arr.size - 1).toBigInteger()
    while ((low).toBigInteger().compareTo((high)) <= 0) {
        var mid: BigInteger = ((low).toBigInteger().add((high))).divide((2).toBigInteger())
        if (arr[(mid).toInt()]!! > x) {
            high = mid.subtract((1).toBigInteger())
        } else {
            if (arr[(mid).toInt()]!! < x) {
                low = (mid.add((1).toBigInteger())).toInt()
            } else {
                return mid.toInt()
            }
        }
    }
    return 0 - 1
}

fun bsearchRec(arr: MutableList<Int>, x: Int, low: Int, high: Int): Int {
    if (high < low) {
        return 0 - 1
    }
    var mid: BigInteger = ((low + high) / 2).toBigInteger()
    if (arr[(mid).toInt()]!! > x) {
        return bsearchRec(arr, x, low, (mid.subtract((1).toBigInteger())).toInt())
    } else {
        if (arr[(mid).toInt()]!! < x) {
            return bsearchRec(arr, x, (mid.add((1).toBigInteger())).toInt(), high)
        }
    }
    return mid.toInt()
}

fun user_main(): Unit {
    var nums: MutableList<Int> = mutableListOf(0 - 31, 0, 1, 2, 2, 4, 65, 83, 99, 782)
    var x: Int = 2
    var idx: Int = bsearch(nums, x)
    if (idx >= 0) {
        println(((x.toString() + " is at index ") + idx.toString()) + ".")
    } else {
        println(x.toString() + " is not found.")
    }
    x = 5
    idx = bsearchRec(nums, x, 0, nums.size - 1)
    if (idx >= 0) {
        println(((x.toString() + " is at index ") + idx.toString()) + ".")
    } else {
        println(x.toString() + " is not found.")
    }
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
