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

var data: String = "01001100100111"
fun to_binary(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var num: Int = n
    var res: String = ""
    while (num > 0) {
        var bit: Int = Math.floorMod(num, 2)
        res = bit.toString() + res
        num = num / 2
    }
    return res
}

fun contains_key_int(m: MutableMap<String, Int>, key: String): Boolean {
    for (k in m.keys.toMutableList()) {
        if (k == key) {
            return true
        }
    }
    return false
}

fun lzw_compress(bits: String): String {
    var dict: MutableMap<String, Int> = (mutableMapOf<String, Int>("0" to (0), "1" to (1)) as MutableMap<String, Int>)
    var current: String = ""
    var result: String = ""
    var index: Int = 2
    var i: Int = 0
    while (i < bits.length) {
        var ch: String = bits[i].toString()
        var candidate: String = current + ch
        if (((contains_key_int(dict, candidate)) as Boolean)) {
            current = candidate
        } else {
            result = result + to_binary((dict)[current] as Int)
            (dict)[candidate] = index
            index = index + 1
            current = ch
        }
        i = i + 1
    }
    if (current != "") {
        result = result + to_binary((dict)[current] as Int)
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(lzw_compress(data))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
