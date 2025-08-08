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

var nums: MutableList<String> = mutableListOf("030", "100", "247", "235", "007")
var t: Int = 0
fun octal_to_hex(octal: String): String {
    var s: String = octal
    if ((((s.length >= 2) && (s[0].toString() == "0") as Boolean)) && (s[1].toString() == "o")) {
        s = s.substring(2, s.length)
    }
    if (s.length == 0) {
        panic("Empty string was passed to the function")
    }
    var j: Int = 0
    while (j < s.length) {
        var c: String = s[j].toString()
        if ((((((((((((((c != "0") && (c != "1") as Boolean)) && (c != "2") as Boolean)) && (c != "3") as Boolean)) && (c != "4") as Boolean)) && (c != "5") as Boolean)) && (c != "6") as Boolean)) && (c != "7")) {
            panic("Not a Valid Octal Number")
        }
        j = j + 1
    }
    var decimal: Int = 0
    var k: Int = 0
    while (k < s.length) {
        var d: Int = s[k].toString().toInt()
        decimal = (decimal * 8) + d
        k = k + 1
    }
    var hex_chars: String = "0123456789ABCDEF"
    if (decimal == 0) {
        return "0x"
    }
    var hex: String = ""
    while (decimal > 0) {
        var idx: Int = Math.floorMod(decimal, 16)
        hex = hex_chars[idx].toString() + hex
        decimal = decimal / 16
    }
    return "0x" + hex
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (t < nums.size) {
            var num: String = nums[t]!!
            println(octal_to_hex(num))
            t = t + 1
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
