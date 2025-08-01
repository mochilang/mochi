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

fun cart2(a: MutableList<Int>, b: MutableList<Int>): MutableList<MutableList<Int>> {
    var p: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (x in a) {
        for (y in b) {
            p = run { val _tmp = p.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp } as MutableList<MutableList<Int>>
        }
    }
    return p
}

fun llStr(lst: MutableList<MutableList<Int>>): String {
    var s: String = "["
    var i: Int = 0
    while (i < lst.size) {
        var row: MutableList<Int> = lst[i]!!
        s = s + "["
        var j: Int = 0
        while (j < row.size) {
            s = s + (row[j]!!).toString()
            if (j < (row.size - 1)) {
                s = s + " "
            }
            j = j + 1
        }
        s = s + "]"
        if (i < (lst.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    println(llStr(cart2(mutableListOf(1, 2), mutableListOf(3, 4))))
    println(llStr(cart2(mutableListOf(3, 4), mutableListOf(1, 2))))
    println(llStr(cart2(mutableListOf(1, 2), mutableListOf<Int>())))
    println(llStr(cart2(mutableListOf<Int>(), mutableListOf(1, 2))))
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
