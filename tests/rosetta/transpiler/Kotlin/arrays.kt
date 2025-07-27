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

var a: MutableList<Int> = mutableListOf(0, 0, 0, 0, 0)
var s: MutableList<Int> = a.subList(0, 4)
var cap_s: Int = 5
fun listStr(xs: MutableList<Int>): String {
    var s: String = "["
    var i: Int = 0
    while (i < xs.size) {
        s = s + (xs[i]).toString()
        if ((i + 1) < xs.size) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("len(a) = " + a.size.toString())
        println("a = " + listStr(a))
        a[0] = 3
        println("a = " + listStr(a))
        println("a[0] = " + (a[0]).toString())
        println("s = " + listStr(s))
        println((("len(s) = " + s.size.toString()) + "  cap(s) = ") + cap_s.toString())
        s = a.subList(0, 5)
        println("s = " + listStr(s))
        a[0] = 22
        s[0] = 22
        println("a = " + listStr(a))
        println("s = " + listStr(s))
        s = run { val _tmp = s.toMutableList(); _tmp.add(4); _tmp }
        s = run { val _tmp = s.toMutableList(); _tmp.add(5); _tmp }
        s = run { val _tmp = s.toMutableList(); _tmp.add(6); _tmp }
        cap_s = 10
        println("s = " + listStr(s))
        println((("len(s) = " + s.size.toString()) + "  cap(s) = ") + cap_s.toString())
        a[4] = 0 - 1
        println("a = " + listStr(a))
        println("s = " + listStr(s))
        s = mutableListOf()
        for (i in 0 until 8) {
            s = run { val _tmp = s.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        }
        cap_s = 8
        println("s = " + listStr(s))
        println((("len(s) = " + s.size.toString()) + "  cap(s) = ") + cap_s.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
