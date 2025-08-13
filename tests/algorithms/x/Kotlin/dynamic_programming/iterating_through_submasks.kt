import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun bitwise_and(a: Int, b: Int): Int {
    var result: Int = (0).toInt()
    var bit: Int = (1).toInt()
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    while ((x > 0) || (y > 0)) {
        var abit: Int = (Math.floorMod(x, 2)).toInt()
        var bbit: Int = (Math.floorMod(y, 2)).toInt()
        if ((abit == 1) && (bbit == 1)) {
            result = result + bit
        }
        x = x / 2
        y = y / 2
        bit = bit * 2
    }
    return result
}

fun list_of_submasks(mask: Int): MutableList<Int> {
    if (mask <= 0) {
        panic("mask needs to be positive integer, your input " + mask.toString())
    }
    var all_submasks: MutableList<Int> = mutableListOf<Int>()
    var submask: Int = (mask).toInt()
    while (submask != 0) {
        all_submasks = run { val _tmp = all_submasks.toMutableList(); _tmp.add(submask); _tmp }
        submask = bitwise_and(submask - 1, mask)
    }
    return all_submasks
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(list_of_submasks(15).toString())
        println(list_of_submasks(13).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
