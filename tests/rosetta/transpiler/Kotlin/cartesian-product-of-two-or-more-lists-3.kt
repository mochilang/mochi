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

fun concat(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    for (v in a) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
    }
    for (v in b) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
    }
    return out
}

fun cartN(lists: Any?): MutableList<MutableList<Int>> {
    if (lists == null) {
        return mutableListOf<MutableList<Int>>()
    }
    var a: MutableList<MutableList<Int>> = lists as MutableList<MutableList<Int>>
    if (a.size == 0) {
        return mutableListOf<MutableList<Int>>(mutableListOf<Int>())
    }
    var out: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var rest: MutableList<MutableList<Int>> = cartN(a.subList(1, a.size) as Any?)
    for (x in a[0]!!) {
        for (p in rest) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(concat(mutableListOf<Int>(x.toInt()), p)); _tmp } as MutableList<MutableList<Int>>
        }
    }
    return out
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
