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

fun bit_xor(a: Int, b: Int): Int {
    var ua: Int = a
    var ub: Int = b
    var res: Int = 0
    var bit: Int = 1
    while ((ua > 0) || (ub > 0)) {
        var abit: Int = Math.floorMod(ua, 2)
        var bbit: Int = Math.floorMod(ub, 2)
        if ((((abit == 1) && (bbit == 0) as Boolean)) || (((abit == 0) && (bbit == 1) as Boolean))) {
            res = res + bit
        }
        ua = ((ua / 2).toInt())
        ub = ((ub / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun find_unique_number(arr: MutableList<Int>): Int {
    if (arr.size == 0) {
        panic("input list must not be empty")
    }
    var result: Int = 0
    for (num in arr) {
        result = bit_xor(result, num)
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_unique_number(mutableListOf(1, 1, 2, 2, 3)).toString())
        println(find_unique_number(mutableListOf(4, 5, 4, 6, 6)).toString())
        println(find_unique_number(mutableListOf(7)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
