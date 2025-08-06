import java.math.BigInteger

fun lshift(x: Int, n: Int): Int {
return (x.toLong() * pow2(n)).toInt()
}

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun rshift(x: Int, n: Int): Int {
return (x.toLong() / pow2(n)).toInt()
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

fun pad_left_num(n: Int): String {
    var s: String = n.toString()
    while (s.length < 5) {
        s = " " + s
    }
    return s
}

fun to_binary(n: Int): String {
    var sign: String = ""
    var num: Int = n
    if (num < 0) {
        sign = "-"
        num = 0 - num
    }
    var bits: String = ""
    while (num > 0) {
        bits = (Math.floorMod(num, 2)).toString() + bits
        num = (num - (Math.floorMod(num, 2))) / 2
    }
    if (bits == "") {
        bits = "0"
    }
    var min_width: Int = 8
    while (bits.length < (min_width - sign.length)) {
        bits = "0" + bits
    }
    return sign + bits
}

fun show_bits(before: Int, after: Int): String {
    return (((((pad_left_num(before) + ": ") + to_binary(before)) + "\n") + pad_left_num(after)) + ": ") + to_binary(after)
}

fun swap_odd_even_bits(num: Int): Int {
    var n: Int = num
    if (n < 0) {
        n = (n + 4294967296L).toInt()
    }
    var result: Int = 0
    var i: Int = 0
    while (i < 32) {
        var bit1: Int = Math.floorMod(rshift((n.toInt()), i), 2)
        var bit2: Int = Math.floorMod(rshift((n.toInt()), i + 1), 2)
        result = (result + lshift(bit1, i + 1)) + lshift(bit2, i)
        i = i + 2
    }
    return result
}

fun user_main(): Unit {
    var nums: MutableList<Int> = mutableListOf(0 - 1, 0, 1, 2, 3, 4, 23, 24)
    var i: Int = 0
    while (i < nums.size) {
        var n: Int = nums[i]!!
        println(show_bits(n, swap_odd_even_bits(n)))
        println("")
        i = i + 1
    }
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
