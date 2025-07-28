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

val n: MutableList<Int> = mutableListOf(3, 5, 7)
val a: MutableList<Int> = mutableListOf(2, 3, 2)
val res: Int = crt(a, n)
fun egcd(a: Int, b: Int): MutableList<Int> {
    if (a == 0) {
        return mutableListOf(b, 0, 1)
    }
    val res: MutableList<Int> = egcd(Math.floorMod(b, a), a)
    val g: Int = res[0]
    val x1: Int = res[1]
    val y1: Int = res[2]
    return mutableListOf(g, y1 - ((b / a) * x1), x1)
}

fun modInv(a: Int, m: Int): Int {
    val r: MutableList<Int> = egcd(a, m)
    if (r[0] != 1) {
        return 0
    }
    val x: Int = r[1]
    if (x < 0) {
        return x + m
    }
    return x
}

fun crt(a: MutableList<Int>, n: MutableList<Int>): Int {
    var prod: Int = 1
    var i: Int = 0
    while (i < n.size) {
        prod = prod * n[i]
        i = i + 1
    }
    var x: Int = 0
    i = 0
    while (i < n.size) {
        val ni: Int = n[i]
        val ai: Int = a[i]
        val p: Int = prod / ni
        val inv: Int = modInv(Math.floorMod(p, ni), ni)
        x = x + ((ai * inv) * p)
        i = i + 1
    }
    return Math.floorMod(x, prod)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(res.toString() + " <nil>")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
