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

fun listStr(xs: MutableList<Int>): String {
    var s: String = "["
    var i: Int = 0
    while (i < xs.size) {
        s = s + (xs[i]!!).toString()
        if (i < (xs.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun ordered(xs: MutableList<Int>): Boolean {
    if (xs.size == 0) {
        return true
    }
    var prev: Int = xs[0]!!
    var i: Int = 1
    while (i < xs.size) {
        if (xs[i]!! < prev) {
            return false
        }
        prev = xs[i]!!
        i = i + 1
    }
    return true
}

fun outOfOrder(n: Int): MutableList<Int> {
    if (n < 2) {
        return mutableListOf<Int>()
    }
    var r: MutableList<Int> = mutableListOf<Int>()
    while (true) {
        r = mutableListOf<Int>()
        var i: Int = 0
        while (i < n) {
            r = run { val _tmp = r.toMutableList(); _tmp.add(Math.floorMod(_now(), 3)); _tmp }
            i = i + 1
        }
        if (!ordered(r)) {
            break
        }
    }
    return r
}

fun sort3(a: MutableList<Int>): MutableList<Int> {
    var lo: Int = 0
    var mid: Int = 0
    var hi: BigInteger = ((a.size - 1).toBigInteger())
    while ((mid).toBigInteger().compareTo((hi)) <= 0) {
        var v: Int = a[mid]!!
        if (v == 0) {
            var tmp: Int = a[lo]!!
            a[lo] = a[mid]!!
            a[mid] = tmp
            lo = lo + 1
            mid = mid + 1
        } else {
            if (v == 1) {
                mid = mid + 1
            } else {
                var tmp: Int = a[mid]!!
                a[mid] = a[(hi).toInt()]!!
                a[(hi).toInt()] = tmp
                hi = hi.subtract((1).toBigInteger())
            }
        }
    }
    return a
}

fun user_main(): Unit {
    var f: MutableList<Int> = outOfOrder(12)
    println(listStr(f))
    f = sort3(f)
    println(listStr(f))
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
