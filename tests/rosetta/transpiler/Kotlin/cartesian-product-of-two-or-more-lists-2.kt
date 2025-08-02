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

fun llStr(lst: MutableList<MutableList<Int>>): String {
    var s: String = "["
    var i: Int = 0
    while (i < lst.size) {
        s = s + listStr(lst[i]!!)
        if (i < (lst.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun cartN(lists: Any?): MutableList<MutableList<Int>> {
    if (lists == null) {
        return mutableListOf<MutableList<Int>>()
    }
    var a: MutableList<MutableList<Int>> = lists as MutableList<MutableList<Int>>
    if (a.size == 0) {
        return mutableListOf<MutableList<Int>>(mutableListOf<Int>())
    }
    var c: Int = 1
    for (xs in a) {
        c = c * xs.size
    }
    if (c == 0) {
        return mutableListOf<MutableList<Int>>()
    }
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var idx: MutableList<Int> = mutableListOf<Int>()
    for (_u1 in a) {
        idx = run { val _tmp = idx.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
    }
    var n: Int = a.size
    var count: Int = 0
    while (count < c) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((a[j]!!) as MutableList<Int>)[idx[j]!!]!!); _tmp } as MutableList<Int>
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        var k: BigInteger = (n - 1).toBigInteger()
        while (k.compareTo(0.toBigInteger()) >= 0) {
            idx[(k).toInt()] = idx[(k).toInt()]!! + 1
            if (idx[(k).toInt()]!! < (a[(k).toInt()]!!).size) {
                break
            }
            idx[(k).toInt()] = 0
            k = k.subtract(1.toBigInteger())
        }
        count = count + 1
    }
    return res
}

fun user_main(): Unit {
    println(llStr(cartN(mutableListOf(mutableListOf(1, 2), mutableListOf(3, 4)) as Any?)))
    println(llStr(cartN(mutableListOf(mutableListOf(3, 4), mutableListOf(1, 2)) as Any?)))
    println(llStr(cartN(mutableListOf(mutableListOf(1, 2), mutableListOf<Int>()) as Any?)))
    println(llStr(cartN(mutableListOf(mutableListOf<Int>(), mutableListOf(1, 2)) as Any?)))
    println("")
    println("[")
    for (p in cartN(mutableListOf(mutableListOf(1776, 1789), mutableListOf(7, 12), mutableListOf(4, 14, 23), mutableListOf(0, 1)) as Any?)) {
        println(" " + listStr(p))
    }
    println("]")
    println(llStr(cartN(mutableListOf(mutableListOf(1, 2, 3), mutableListOf(30), mutableListOf(500, 100)) as Any?)))
    println(llStr(cartN(mutableListOf(mutableListOf(1, 2, 3), mutableListOf<Int>(), mutableListOf(500, 100)) as Any?)))
    println("")
    println(llStr(cartN(null as Any?)))
    println(llStr(cartN(mutableListOf<Any?>() as Any?)))
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
