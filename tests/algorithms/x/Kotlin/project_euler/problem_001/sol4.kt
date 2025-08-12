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

fun contains(xs: MutableList<Int>, value: Int): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == value) {
            return true
        }
        i = i + 1
    }
    return false
}

fun solution(n: Int): Int {
    var zmulti: MutableList<Int> = mutableListOf<Int>()
    var xmulti: MutableList<Int> = mutableListOf<Int>()
    var temp: Int = (1).toInt()
    while (true) {
        var result: Int = (3 * temp).toInt()
        if (result < n) {
            zmulti = run { val _tmp = zmulti.toMutableList(); _tmp.add(result); _tmp }
            temp = temp + 1
        } else {
            break
        }
    }
    temp = 1
    while (true) {
        var result: Int = (5 * temp).toInt()
        if (result < n) {
            xmulti = run { val _tmp = xmulti.toMutableList(); _tmp.add(result); _tmp }
            temp = temp + 1
        } else {
            break
        }
    }
    var collection: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < zmulti.size) {
        var v: Int = (zmulti[i]!!).toInt()
        if (!((collection.contains(v)) as Boolean)) {
            collection = run { val _tmp = collection.toMutableList(); _tmp.add(v); _tmp }
        }
        i = i + 1
    }
    i = 0
    while (i < xmulti.size) {
        var v: Int = (xmulti[i]!!).toInt()
        if (!((collection.contains(v)) as Boolean)) {
            collection = run { val _tmp = collection.toMutableList(); _tmp.add(v); _tmp }
        }
        i = i + 1
    }
    var total: Int = (0).toInt()
    i = 0
    while (i < collection.size) {
        total = total + collection[i]!!
        i = i + 1
    }
    return total
}

fun test_solution(): Unit {
    if (solution(3) != 0) {
        panic("solution(3) failed")
    }
    if (solution(4) != 3) {
        panic("solution(4) failed")
    }
    if (solution(10) != 23) {
        panic("solution(10) failed")
    }
    if (solution(600) != 83700) {
        panic("solution(600) failed")
    }
}

fun user_main(): Unit {
    test_solution()
    println("solution() = " + solution(1000).toString())
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
