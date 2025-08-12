import java.math.BigInteger

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
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

var p4_table: MutableList<Int> = mutableListOf(2, 4, 3, 1)
var key: String = "1010000010"
var message: String = "11010111"
var p8_table: MutableList<Int> = mutableListOf(6, 3, 7, 4, 8, 5, 10, 9)
var p10_table: MutableList<Int> = mutableListOf(3, 5, 2, 7, 4, 10, 1, 9, 8, 6)
var IP: MutableList<Int> = mutableListOf(2, 6, 3, 1, 4, 8, 5, 7)
var IP_inv: MutableList<Int> = mutableListOf(4, 1, 3, 5, 7, 2, 8, 6)
var expansion: MutableList<Int> = mutableListOf(4, 1, 2, 3, 2, 3, 4, 1)
var s0: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 0, 3, 2), mutableListOf(3, 2, 1, 0), mutableListOf(0, 2, 1, 3), mutableListOf(3, 1, 3, 2))
var s1: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 2, 3), mutableListOf(2, 0, 1, 3), mutableListOf(3, 0, 1, 0), mutableListOf(2, 1, 0, 3))
var temp: String = apply_table(key, p10_table)
var left: String = _sliceStr(temp, 0, 5)
var right: String = _sliceStr(temp, 5, 10)
fun apply_table(inp: String, table: MutableList<Int>): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < table.size) {
        var idx: BigInteger = ((table[i]!! - 1).toBigInteger())
        if (idx.compareTo((0).toBigInteger()) < 0) {
            idx = ((inp.length - 1).toBigInteger())
        }
        res = res + _sliceStr(inp, (idx).toInt(), (idx.add((1).toBigInteger())).toInt())
        i = i + 1
    }
    return res
}

fun left_shift(data: String): String {
    return _sliceStr(data, 1, data.length) + _sliceStr(data, 0, 1)
}

fun xor(a: String, b: String): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while ((i < a.length) && (i < b.length)) {
        if (_sliceStr(a, i, i + 1) == _sliceStr(b, i, i + 1)) {
            res = res + "0"
        } else {
            res = res + "1"
        }
        i = i + 1
    }
    return res
}

fun int_to_binary(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var res: String = ""
    var num: Int = (n).toInt()
    while (num > 0) {
        res = (Math.floorMod(num, 2)).toString() + res
        num = num / 2
    }
    return res
}

fun pad_left(s: String, width: Int): String {
    var res: String = s
    while (res.length < width) {
        res = "0" + res
    }
    return res
}

fun bin_to_int(s: String): Int {
    var result: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var digit: Int = ((_sliceStr(s, i, i + 1).toBigInteger().toInt())).toInt()
        result = (result * 2) + digit
        i = i + 1
    }
    return result
}

fun apply_sbox(s: MutableList<MutableList<Int>>, data: String): String {
    var row_bits: String = _sliceStr(data, 0, 1) + _sliceStr(data, data.length - 1, data.length)
    var col_bits: String = _sliceStr(data, 1, 3)
    var row: Int = (bin_to_int(row_bits)).toInt()
    var col: Int = (bin_to_int(col_bits)).toInt()
    var _val: Int = ((((s[row]!!) as MutableList<Int>))[col]!!).toInt()
    var out: String = int_to_binary(_val)
    return out
}

fun f(expansion: MutableList<Int>, s0: MutableList<MutableList<Int>>, s1: MutableList<MutableList<Int>>, key: String, message: String): String {
    var left: String = _sliceStr(message, 0, 4)
    var right: String = _sliceStr(message, 4, 8)
    var temp: String = apply_table(right, expansion)
    temp = xor(temp, key)
    var left_bin_str: String = apply_sbox(s0, _sliceStr(temp, 0, 4))
    var right_bin_str: String = apply_sbox(s1, _sliceStr(temp, 4, 8))
    left_bin_str = pad_left(left_bin_str, 2)
    right_bin_str = pad_left(right_bin_str, 2)
    temp = apply_table(left_bin_str + right_bin_str, p4_table)
    temp = xor(left, temp)
    return temp + right
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        left = left_shift(left)
        right = left_shift(right)
        var key1: String = apply_table(left + right, p8_table)
        left = left_shift(left)
        right = left_shift(right)
        left = left_shift(left)
        right = left_shift(right)
        var key2: String = apply_table(left + right, p8_table)
        temp = apply_table(message, IP)
        temp = f(expansion, s0, s1, key1, temp)
        temp = _sliceStr(temp, 4, 8) + _sliceStr(temp, 0, 4)
        temp = f(expansion, s0, s1, key2, temp)
        var CT: String = apply_table(temp, IP_inv)
        println("Cipher text is: " + CT)
        temp = apply_table(CT, IP)
        temp = f(expansion, s0, s1, key2, temp)
        temp = _sliceStr(temp, 4, 8) + _sliceStr(temp, 0, 4)
        temp = f(expansion, s0, s1, key1, temp)
        var PT: String = apply_table(temp, IP_inv)
        println("Plain text after decypting is: " + PT)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
