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

var a: MutableList<Int> = mutableListOf(1, 2, 3)
var b: MutableList<Int> = mutableListOf(7, 12, 60)
var i: MutableList<Any?> = mutableListOf(1, 2, 3)
var j: MutableList<Any?> = mutableListOf("Crosby", "Stills", "Nash", "Young")
var l: MutableList<Int> = mutableListOf(1, 2, 3)
var m: MutableList<Int> = mutableListOf(7, 12, 60)
fun concatInts(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf()
    for (v in a) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
    }
    for (v in b) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
    }
    return out
}

fun concatAny(a: MutableList<Any?>, b: MutableList<Any?>): MutableList<Any?> {
    var out: MutableList<Any?> = mutableListOf()
    for (v in a) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp } as MutableList<Any?>
    }
    for (v in b) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp } as MutableList<Any?>
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(concatInts(a, b).toString())
        println(concatAny(i, j).toString())
        println(concatInts(l, m).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
