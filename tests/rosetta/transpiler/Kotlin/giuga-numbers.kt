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

fun primeFactors(n: Int): MutableList<Int> {
    var factors: MutableList<Int> = mutableListOf<Int>()
    var last: Int = 0
    var x: Int = n
    while ((Math.floorMod(x, 2)) == 0) {
        if (last == 2) {
            return mutableListOf<Int>()
        }
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(2); _tmp } as MutableList<Int>
        last = 2
        x = x / 2
    }
    var p: Int = 3
    while ((p * p) <= x) {
        while ((Math.floorMod(x, p)) == 0) {
            if (last == p) {
                return mutableListOf<Int>()
            }
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(p); _tmp } as MutableList<Int>
            last = p
            x = x / p
        }
        p = p + 2
    }
    if (x > 1) {
        if (last == x) {
            return mutableListOf<Int>()
        }
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
    }
    return factors
}

fun isGiuga(n: Int): Boolean {
    var facs: MutableList<Int> = primeFactors(n)
    if (facs.size <= 2) {
        return false
    }
    for (f in facs) {
        if ((Math.floorMod(((n / f) - 1), f)) != 0) {
            return false
        }
    }
    return true
}

fun user_main(): Unit {
    var known: MutableList<Int> = mutableListOf(30, 858, 1722, 66198)
    var nums: MutableList<Int> = mutableListOf<Int>()
    for (n in known) {
        if ((isGiuga(n)) as Boolean) {
            nums = run { val _tmp = nums.toMutableList(); _tmp.add(n); _tmp } as MutableList<Int>
        }
    }
    println("The first 4 Giuga numbers are:")
    println(nums)
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
