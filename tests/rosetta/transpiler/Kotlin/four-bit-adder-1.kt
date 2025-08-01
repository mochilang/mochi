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

data class SumCarry(var s: Boolean, var c: Boolean)
data class Add4Result(var v: Boolean, var s3: Boolean, var s2: Boolean, var s1: Boolean, var s0: Boolean)
fun xor(a: Boolean, b: Boolean): Boolean {
    return (((a && (!b as Boolean) as Boolean)) || (((!a as Boolean) && b as Boolean))) as Boolean
}

fun ha(a: Boolean, b: Boolean): SumCarry {
    return SumCarry(s = xor(a, b), c = a && b)
}

fun fa(a: Boolean, b: Boolean, c0: Boolean): SumCarry {
    var r1: SumCarry = ha(a, c0)
    var r2: SumCarry = ha(r1.s, b)
    return SumCarry(s = r2.s, c = r1.c || r2.c)
}

fun add4(a3: Boolean, a2: Boolean, a1: Boolean, a0: Boolean, b3: Boolean, b2: Boolean, b1: Boolean, b0: Boolean): Add4Result {
    var r0: SumCarry = fa(a0, b0, false)
    var r1: SumCarry = fa(a1, b1, r0.c)
    var r2: SumCarry = fa(a2, b2, r1.c)
    var r3: SumCarry = fa(a3, b3, r2.c)
    return Add4Result(v = r3.c, s3 = r3.s, s2 = r2.s, s1 = r1.s, s0 = r0.s)
}

fun b2i(b: Boolean): Int {
    if (b as Boolean) {
        return 1
    }
    return 0
}

fun user_main(): Unit {
    var r: Add4Result = add4(true, false, true, false, true, false, false, true)
    println((((((((b2i(r.v).toString() + " ") + b2i(r.s3).toString()) + " ") + b2i(r.s2).toString()) + " ") + b2i(r.s1).toString()) + " ") + b2i(r.s0).toString())
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
