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

fun binary_multiply(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    var res: Int = (0).toInt()
    while (y > 0) {
        if ((Math.floorMod(y, 2)) == 1) {
            res = res + x
        }
        x = x + x
        y = ((y / 2).toInt())
    }
    return res
}

fun binary_mod_multiply(a: Int, b: Int, modulus: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    var res: Int = (0).toInt()
    while (y > 0) {
        if ((Math.floorMod(y, 2)) == 1) {
            res = Math.floorMod(((Math.floorMod(res, modulus)) + (Math.floorMod(x, modulus))), modulus)
        }
        x = x + x
        y = ((y / 2).toInt())
    }
    return Math.floorMod(res, modulus)
}

fun user_main(): Unit {
    println(binary_multiply(2, 3).toString())
    println(binary_multiply(5, 0).toString())
    println(binary_mod_multiply(2, 3, 5).toString())
    println(binary_mod_multiply(10, 5, 13).toString())
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
