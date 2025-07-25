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

fun pfacSum(i: Int): Int {
    var sum: Int = 0
    var p: Int = 1
    while (p <= (i / 2)) {
        if ((i % p) == 0) {
            sum = sum + p
        }
        p = p + 1
    }
    return sum
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    var sums: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < 20000) {
        sums = run { val _tmp = sums.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    i = 1
    while (i < 20000) {
        sums[i] = pfacSum(i)
        i = i + 1
    }
    println("The amicable pairs below 20,000 are:")
    var n: Int = 2
    while (n < 19999) {
        val m: Int = sums[n]
        if ((((m > n) && (m < 20000) as Boolean)) && (n == sums[m])) {
            println((("  " + pad(n, 5)) + " and ") + pad(m, 5))
        }
        n = n + 1
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
