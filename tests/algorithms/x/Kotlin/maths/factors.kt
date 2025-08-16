import java.math.BigInteger

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

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

fun reverse(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (xs.size - 1).toInt()
    while (i >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i - 1
    }
    return res
}

fun factors_of_a_number(num: Int): MutableList<Int> {
    var facs: MutableList<Int> = mutableListOf<Int>()
    if (num < 1) {
        return facs
    }
    var small: MutableList<Int> = mutableListOf<Int>()
    var large: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while ((i * i) <= num) {
        if ((Math.floorMod(num, i)) == 0) {
            small = run { val _tmp = small.toMutableList(); _tmp.add(i); _tmp }
            var d: Int = (num / i).toInt()
            if (d != i) {
                large = run { val _tmp = large.toMutableList(); _tmp.add(d); _tmp }
            }
        }
        i = i + 1
    }
    facs = concat(small, reverse(large))
    return facs
}

fun run_tests(): Unit {
    if (factors_of_a_number(1) != mutableListOf(1)) {
        panic("case1 failed")
    }
    if (factors_of_a_number(5) != mutableListOf(1, 5)) {
        panic("case2 failed")
    }
    if (factors_of_a_number(24) != mutableListOf(1, 2, 3, 4, 6, 8, 12, 24)) {
        panic("case3 failed")
    }
    if (factors_of_a_number(0 - 24) != mutableListOf<Any?>()) {
        panic("case4 failed")
    }
}

fun user_main(): Unit {
    run_tests()
    println(factors_of_a_number(24).toString())
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
