import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun multiplicative_persistence(num: Int): Int {
    if (num < 0) {
        panic("multiplicative_persistence() does not accept negative values")
    }
    var steps: Int = (0).toInt()
    var n: Int = (num).toInt()
    while (n >= 10) {
        var product: Int = (1).toInt()
        var temp: Int = (n).toInt()
        while (temp > 0) {
            var digit: Int = (Math.floorMod(temp, 10)).toInt()
            product = product * digit
            temp = temp / 10
        }
        n = product
        steps = steps + 1
    }
    return steps
}

fun additive_persistence(num: Int): Int {
    if (num < 0) {
        panic("additive_persistence() does not accept negative values")
    }
    var steps: Int = (0).toInt()
    var n: Int = (num).toInt()
    while (n >= 10) {
        var total: Int = (0).toInt()
        var temp: Int = (n).toInt()
        while (temp > 0) {
            var digit: Int = (Math.floorMod(temp, 10)).toInt()
            total = total + digit
            temp = temp / 10
        }
        n = total
        steps = steps + 1
    }
    return steps
}

fun test_persistence(): Unit {
    if (multiplicative_persistence(217) != 2) {
        panic("multiplicative_persistence failed")
    }
    if (additive_persistence(199) != 3) {
        panic("additive_persistence failed")
    }
}

fun user_main(): Unit {
    test_persistence()
    println(_numToStr(multiplicative_persistence(217)))
    println(_numToStr(additive_persistence(199)))
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
