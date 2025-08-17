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

fun bubble_sort(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = xs
    var n: Int = (arr.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var j: Int = (0).toInt()
        while (j < ((n - i) - 1)) {
            if (arr[j]!! > arr[j + 1]!!) {
                var tmp: Int = (arr[j]!!).toInt()
                _listSet(arr, j, arr[j + 1]!!)
                _listSet(arr, j + 1, tmp)
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun factors(num: Int): MutableList<Int> {
    var values: MutableList<Int> = mutableListOf(1)
    var i: Int = (2).toInt()
    while ((i * i) <= num) {
        if ((Math.floorMod(num, i)) == 0) {
            values = run { val _tmp = values.toMutableList(); _tmp.add(i); _tmp }
            var d: Int = (num / i).toInt()
            if (d != i) {
                values = run { val _tmp = values.toMutableList(); _tmp.add(d); _tmp }
            }
        }
        i = i + 1
    }
    return bubble_sort(values)
}

fun sum_list(xs: MutableList<Int>): Int {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        total = total + xs[i]!!
        i = i + 1
    }
    return total
}

fun abundant(n: Int): Boolean {
    return sum_list(factors(n)) > n
}

fun semi_perfect(number: Int): Boolean {
    if (number <= 0) {
        return true
    }
    var values: MutableList<Int> = factors(number)
    var possible: MutableList<Boolean> = mutableListOf<Boolean>()
    var j: Int = (0).toInt()
    while (j <= number) {
        possible = run { val _tmp = possible.toMutableList(); _tmp.add(j == 0); _tmp }
        j = j + 1
    }
    var idx: Int = (0).toInt()
    while (idx < values.size) {
        var v: Int = (values[idx]!!).toInt()
        var s: Int = (number).toInt()
        while (s >= v) {
            if ((possible[s - v]!!) as Boolean) {
                _listSet(possible, s, true)
            }
            s = s - 1
        }
        idx = idx + 1
    }
    return possible[number]!!
}

fun weird(number: Int): Boolean {
    return (abundant(number) && (semi_perfect(number) == false)) as Boolean
}

fun run_tests(): Unit {
    if (factors(12) != mutableListOf(1, 2, 3, 4, 6)) {
        panic("factors 12 failed")
    }
    if (factors(1) != mutableListOf(1)) {
        panic("factors 1 failed")
    }
    if (factors(100) != mutableListOf(1, 2, 4, 5, 10, 20, 25, 50)) {
        panic("factors 100 failed")
    }
    if (abundant(0) != true) {
        panic("abundant 0 failed")
    }
    if (abundant(1) != false) {
        panic("abundant 1 failed")
    }
    if (abundant(12) != true) {
        panic("abundant 12 failed")
    }
    if (abundant(13) != false) {
        panic("abundant 13 failed")
    }
    if (abundant(20) != true) {
        panic("abundant 20 failed")
    }
    if (semi_perfect(0) != true) {
        panic("semi_perfect 0 failed")
    }
    if (semi_perfect(1) != true) {
        panic("semi_perfect 1 failed")
    }
    if (semi_perfect(12) != true) {
        panic("semi_perfect 12 failed")
    }
    if (semi_perfect(13) != false) {
        panic("semi_perfect 13 failed")
    }
    if (weird(0) != false) {
        panic("weird 0 failed")
    }
    if (weird(70) != true) {
        panic("weird 70 failed")
    }
    if (weird(77) != false) {
        panic("weird 77 failed")
    }
}

fun user_main(): Unit {
    run_tests()
    var nums: MutableList<Int> = mutableListOf(69, 70, 71)
    var i: Int = (0).toInt()
    while (i < nums.size) {
        var n: Int = (nums[i]!!).toInt()
        if ((weird(n)) as Boolean) {
            println(_numToStr(n) + " is weird.")
        } else {
            println(_numToStr(n) + " is not weird.")
        }
        i = i + 1
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
