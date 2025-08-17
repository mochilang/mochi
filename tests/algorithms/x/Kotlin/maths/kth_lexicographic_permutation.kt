import java.math.BigInteger

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

fun remove_at(xs: MutableList<Int>, idx: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i != idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun kth_permutation(k: Int, n: Int): MutableList<Int> {
    var k: Int = (k).toInt()
    if (n <= 0) {
        panic("n must be positive")
    }
    var factorials: MutableList<Int> = mutableListOf(1)
    var i: Int = (2).toInt()
    while (i < n) {
        factorials = run { val _tmp = factorials.toMutableList(); _tmp.add(factorials[factorials.size - 1]!! * i); _tmp }
        i = i + 1
    }
    var total: Int = (factorials[factorials.size - 1]!! * n).toInt()
    if ((k < 0) || (k >= total)) {
        panic("k out of bounds")
    }
    var elements: MutableList<Int> = mutableListOf<Int>()
    var e: Int = (0).toInt()
    while (e < n) {
        elements = run { val _tmp = elements.toMutableList(); _tmp.add(e); _tmp }
        e = e + 1
    }
    var permutation: MutableList<Int> = mutableListOf<Int>()
    var idx: Int = (factorials.size - 1).toInt()
    while (idx >= 0) {
        var factorial: Int = (factorials[idx]!!).toInt()
        var number: Int = (k / factorial).toInt()
        k = Math.floorMod(k, factorial)
        permutation = run { val _tmp = permutation.toMutableList(); _tmp.add(elements[number]!!); _tmp }
        elements = remove_at(elements, number)
        idx = idx - 1
    }
    permutation = run { val _tmp = permutation.toMutableList(); _tmp.add(elements[0]!!); _tmp }
    return permutation
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

fun list_to_string(xs: MutableList<Int>): String {
    if (xs.size == 0) {
        return "[]"
    }
    var s: String = "[" + _numToStr(xs[0]!!)
    var i: Int = (1).toInt()
    while (i < xs.size) {
        s = (s + ", ") + _numToStr(xs[i]!!)
        i = i + 1
    }
    s = s + "]"
    return s
}

fun test_kth_permutation(): Unit {
    var expected1: MutableList<Int> = mutableListOf(0, 1, 2, 3, 4)
    var res1: MutableList<Int> = kth_permutation(0, 5)
    if (!list_equal(res1, expected1)) {
        panic("test case 1 failed")
    }
    var expected2: MutableList<Int> = mutableListOf(1, 3, 0, 2)
    var res2: MutableList<Int> = kth_permutation(10, 4)
    if (!list_equal(res2, expected2)) {
        panic("test case 2 failed")
    }
}

fun user_main(): Unit {
    test_kth_permutation()
    var res: MutableList<Int> = kth_permutation(10, 4)
    println(list_to_string(res))
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
