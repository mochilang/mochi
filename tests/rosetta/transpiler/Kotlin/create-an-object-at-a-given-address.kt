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

fun pointerDemo(): Unit {
    println("Pointer:")
    var i: Int = 0
    println("Before:")
    println((("\t<address>: " + i.toString()) + ", ") + i.toString())
    i = 3
    println("After:")
    println((("\t<address>: " + i.toString()) + ", ") + i.toString())
}

fun sliceDemo(): Unit {
    println("Slice:")
    var a = mutableListOf<Any?>()
    for (_u1 in 0 until 10) {
        a = run { val _tmp = a.toMutableList(); _tmp.add((0 as Any?)); _tmp }
    }
    var s: MutableList<Any?> = a
    println("Before:")
    println("\ts: " + listStr((s as MutableList<Int>)))
    println("\ta: " + listStr((a as MutableList<Int>)))
    var data: MutableList<Int> = mutableListOf(65, 32, 115, 116, 114, 105, 110, 103, 46)
    var idx: Int = 0
    while (idx < data.size) {
        s[idx] = ((data[idx]!!) as Any?)
        idx = idx + 1
    }
    println("After:")
    println("\ts: " + listStr((s as MutableList<Int>)))
    println("\ta: " + listStr((a as MutableList<Int>)))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        pointerDemo()
        println("")
        sliceDemo()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
